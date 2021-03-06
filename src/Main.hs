{-# LANGUAGE OverloadedStrings, LambdaCase, PatternGuards, RecordWildCards #-}
module Main where

import Control.Monad
import Control.Applicative
import System.Exit
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.IORef

import Graphics.Vty.Widgets.All hiding (state)
import Graphics.Vty.Widgets.HeaderList
import Graphics.Vty.Widgets.Vim
import Graphics.Vty

import DBusBrowser.DBus
import DBusBrowser.Formatting

main :: IO ()
main = do
  state <- newIORef Nothing
  ui <- newCollection
  (systemList, sessionList) <- createBusLists state ui
  objectsList <- createServiceBrowser state ui

  onList sessionList $ \l -> l `onItemActivated` \(ActivateItemEvent _ name _) -> do
    readIORef state >>= \case
      Nothing -> return ()
      Just bus -> do
        populateObjects objectsList bus name
        setCurrentEntry ui 1
        focus objectsList
        return ()

  onList systemList $ \l -> l `onItemActivated` \(ActivateItemEvent _ name _) -> do
    readIORef state >>= \case
      Nothing -> return ()
      Just bus -> do
        populateObjects objectsList bus name
        setCurrentEntry ui 1
        focus objectsList
        return ()

  runUi ui defaultContext

createServiceBrowser :: IORef (Maybe Client) -> Collection
                     -> IO (Widget (HeaderList (BusName, ObjectPath) FormattedText))
createServiceBrowser state ui = do
  objectsList <- newHeaderList "Objects" 1
  ifaceList <- newHeaderList "Interfaces" 1
  memberList <- newHeaderList "Members" 2

  setFocusAttrs objectsList >> addVimBindings objectsList
  setFocusAttrs ifaceList >> addVimBindings ifaceList
  setFocusAttrs memberList >> addVimBindings memberList

  statusBar <- plainText ""

  onList objectsList $ \l -> l `onSelectionChange` \case
    SelectionOff -> clearListWithHandlers ifaceList
    SelectionOn _ (name, path) _ -> do
      Just bus <- readIORef state
      populateIfaces ifaceList bus name path

  onList ifaceList $ \l -> l `onSelectionChange` \case
    SelectionOff -> clearListWithHandlers memberList
    SelectionOn _ iface _ -> do
      Just bus <- readIORef state
      Just (_, ((name,path), _)) <- onList objectsList getSelected
      populateMembers memberList bus name path iface

  onList memberList $ \l -> l `onSelectionChange` \case
    SelectionOff -> setText statusBar ""
    SelectionOn _ (Property p) _ -> setText statusBar $ showProp p
    SelectionOn _ (Method m) _ -> setTextWithAttrs statusBar $ showMethod m
    SelectionOn _ (Signal s) _ -> setText statusBar $ showSignal s

  fg <- newFocusGroup
  void $ addToFocusGroup fg objectsList
  void $ addToFocusGroup fg ifaceList
  void $ addToFocusGroup fg memberList

  addVimBindings fg

  fg `onKeyPressed` \_ key _ ->
    if key == KChar 'q'
    then setCurrentEntry ui 0 >> return True
    else return False

  bottomBox <- return ifaceList <++> vBorder <++> return memberList
  layout <- return objectsList <--> hBorder <--> return bottomBox
  setBoxChildSizePolicy layout (Percentage 40)
  withBar <- return layout <--> hBorder <--> return statusBar
  void $ addToCollection ui withBar fg

  return objectsList

createBusLists :: IORef (Maybe Client) -> Collection
               -> IO (Widget (HeaderList BusName FormattedText),
                      Widget (HeaderList BusName FormattedText))
createBusLists state ui = do
  sessionList <- newHeaderList "Session Bus" 1
  systemList <- newHeaderList "System Bus" 1

  setFocusAttrs sessionList >> addVimBindings sessionList
  setFocusAttrs systemList >> addVimBindings systemList

  fg <- newFocusGroup
  void $ addToFocusGroup fg sessionList
  void $ addToFocusGroup fg systemList

  addVimBindings fg

  fg `onKeyPressed` \_ key _ ->
    if key == KChar 'q' then
      exitSuccess else return False

  layout <- return sessionList <++> vBorder <++> return systemList

  void $ addToCollection ui layout fg

  (sysBus, sessBus) <- getBusses
  populateList systemList sysBus
  populateList sessionList sessBus

  -- set session bus as first selected bus
  writeIORef state sessBus

  sessionList `onGainFocus` \_ ->
    writeIORef state sessBus

  systemList `onGainFocus` \_ ->
    writeIORef state sysBus

  return (systemList, sessionList)


populateList :: Widget (HeaderList BusName FormattedText) -> Maybe Client -> IO ()
populateList _ Nothing = return ()
populateList list (Just bus) = do
  names <- sortBy compareNames <$> getNames bus
  forM_ names $ \name -> do
    txt <- plainText (T.pack $ formatBusName name)
    onList list $ \l -> addToList l name txt

populateObjects :: Widget (HeaderList (BusName, ObjectPath) FormattedText)
                -> Client -> BusName -> IO ()
populateObjects list bus name = do
  objects <- getObjects bus name
  let prefix = commonPrefix $ map (T.pack . formatObjectPath) objects
  clearListWithHandlers list
  setHeader list $ "Objects " `T.append` prefix
  forM_ objects $ \path -> do
    let path' = stripPrefix' prefix $ T.pack $ formatObjectPath path
    txt <- plainText path'
    onList list $ \l -> addToList l (name, path) txt

populateIfaces :: Widget (HeaderList InterfaceName FormattedText)
               -> Client -> BusName -> ObjectPath -> IO ()
populateIfaces list bus name path = do
  ifaces <- getInterfaces bus name path
  let prefix = commonPrefix $ map (T.pack . formatInterfaceName) ifaces
  clearListWithHandlers list
  setHeader list $ "Interfaces " `T.append` prefix
  forM_ ifaces $ \iface -> do
    let iface' = stripPrefix' prefix $ T.pack $ formatInterfaceName iface
    txt <- plainText iface'
    onList list $ \l -> addToList l iface txt

data SomeMember = Method Method
                | Signal Signal
                | Property Prop

populateMembers :: Widget (HeaderList SomeMember FormattedText)
                -> Client -> BusName -> ObjectPath -> InterfaceName -> IO ()
populateMembers list bus name path iface = do
  clearListWithHandlers list
  getMembers bus name path iface >>= \case
    Nothing -> return ()
    Just (Iface meths sigs props) -> do
      forM_ props $ \prop -> do
        let Just variant = propValue prop
        txt <- plainTextWithAttrs $
               [ (T.concat ["P ", (propName prop)], defAttr)
               , (T.concat [" :: ", (formatType (propType prop)), "\n  "], defAttr `withStyle` dim)
               , (formatVariant variant, defAttr `withStyle` dim)
               ]
        onList list $ \l -> addToList l (Property prop) txt

      forM_ meths $ \meth -> do
        txt <- plainTextWithAttrs $
               (T.concat ["M ", (T.pack $ formatMemberName $ methodName meth), "\n  "], defAttr)
               : map (\(x,y) -> (x, y)) (formatMethType False meth)
        onList list $ \l -> addToList l (Method meth) txt

      forM_ sigs $ \sig -> do
        txt <- plainTextWithAttrs $
               [ (T.concat ["S ", (T.pack $ formatMemberName $ signalName sig), "\n  "], defAttr)
               , (formatSigType False sig, defAttr `withStyle` dim)
               ]
        onList list $ \l -> addToList l (Signal sig) txt

formatSigType :: Bool -> Signal -> Text
formatSigType showNames s = T.intercalate ", " args
  where args = map showArg (signalArgs s)
        showArg a = argText (signalArgName a) (signalArgType a)
        argText name typ
          | showNames = T.concat [nameOrUnderscore name, " :: ", formatType typ]
          | otherwise = formatType typ

formatMethType :: Bool -> Method -> [(Text, Attr)]
formatMethType showNames m = intersperse (", ", defAttr) args
  where showArg a = (argText (methodArgName a) (methodArgType a), argAttr a)
        argText name typ
          | showNames = T.concat [nameOrUnderscore name, " :: ", formatType typ]
          | otherwise = formatType typ
        argAttr a
          | methodArgDirection a == directionIn = withForeColor defAttr green
          | otherwise                           = withForeColor defAttr red
        args = map showArg (methodArgs m)

showProp :: Prop -> Text
showProp Prop{..}
  | Just variant <- propValue = T.concat
                                  [ propName
                                  , ": "
                                  , formatVariant variant
                                  , " :: "
                                  , formatType propType
                                  ]
showProp _ = ""

showMethod :: Method -> [(Text, Attr)]
showMethod m = (T.append mname ": ", defAttr) : formatMethType True m
  where mname = T.pack $ formatMemberName $ methodName m

showSignal :: Signal -> Text
showSignal s = T.concat [sname,  ": ", formatSigType True s]
  where sname = T.pack $ formatMemberName $ signalName s

nameOrUnderscore :: String -> Text
nameOrUnderscore "" = "_"
nameOrUnderscore s  = T.pack s

-- Sort alphabetically, but put names starting with ':' last
compareNames :: BusName -> BusName -> Ordering
compareNames name1 name2 = case (formatBusName name1, formatBusName name2) of
  (':':_, ':':_) -> compare name1 name2
  (':':_, _)     -> GT
  (_, ':':_)     -> LT
  _              -> compare name1 name2

commonPrefix :: [Text] -> Text
commonPrefix []  = ""
commonPrefix lst = foldl1 commonPrefix1 lst
  where commonPrefix1 t1 t2 = case T.commonPrefixes t1 t2 of
          Nothing -> ""
          Just (p, _, _) -> p

stripPrefix' :: Text -> Text -> Text
stripPrefix' pref t = case T.stripPrefix pref t of
  Nothing -> t
  Just "" -> "."
  Just x  -> x

selectedFocusAttr :: Attr
selectedFocusAttr = withBackColor (defAttr `withForeColor` black)  red

selectedUnfocusAttr :: Attr
selectedUnfocusAttr = withBackColor defAttr brightBlack

setFocusAttrs :: Widget (HeaderList a b) -> IO ()
setFocusAttrs list = do
    onList list $ \l -> setSelectedFocusedAttr l (Just selectedFocusAttr)
    onList list $ \l -> setSelectedUnfocusedAttr l (Just selectedUnfocusAttr)
