{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Control.Monad
import Control.Applicative
import System.Exit
import qualified Data.Text as T
import Data.List
import Data.IORef

import Graphics.Vty.Widgets.All hiding (state)
import Graphics.Vty.Widgets.HeaderList
import Graphics.Vty

import DBusBrowser.DBus

-- TODO Add IORef with currently selected bus

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
        return ()

  onList systemList $ \l -> l `onItemActivated` \(ActivateItemEvent _ name _) -> do
    readIORef state >>= \case
      Nothing -> return ()
      Just bus -> do
        populateObjects objectsList bus name
        setCurrentEntry ui 1
        return ()

  runUi ui defaultContext

createServiceBrowser :: IORef (Maybe Client) -> Collection
                     -> IO (Widget (HeaderList ObjectPath FormattedText))
createServiceBrowser state ui = do
  objectsList <- newHeaderList "Objects" 1
  ifaceList <- newHeaderList "Interfaces" 1
  memberList <- newHeaderList "Members" 1

  onList objectsList $ \l -> l `onSelectionChange` \case
    SelectionOff -> onList ifaceList clearList
    SelectionOn _ _ _ -> return () -- TODO

  let foo = "foo"
      bar = "bar"
  onList ifaceList $ \l -> addToList l foo =<< plainText foo
  onList ifaceList $ \l -> addToList l foo =<< plainText bar
  onList memberList $ \l -> addToList l foo =<< plainText foo
  onList memberList $ \l -> addToList l foo =<< plainText bar

  fg <- newFocusGroup
  void $ addToFocusGroup fg objectsList
  void $ addToFocusGroup fg ifaceList
  void $ addToFocusGroup fg memberList

  fg `onKeyPressed` \_ key _ ->
    if key == KChar 'q'
    then setCurrentEntry ui 0 >> return True
    else return False

  fstBox <- return objectsList <++> vBorder <++> return ifaceList
  sndBox <- vBorder <++> return memberList
  layout <- hBox fstBox sndBox
  setBoxChildSizePolicy layout (Percentage 65)
  void $ addToCollection ui layout fg

  return objectsList

createBusLists :: IORef (Maybe Client) -> Collection
               -> IO (Widget (HeaderList BusName FormattedText),
                      Widget (HeaderList BusName FormattedText))
createBusLists state ui = do
  sessionList <- newHeaderList "Session Bus" 1
  systemList <- newHeaderList "System Bus" 1

  fg <- newFocusGroup
  void $ addToFocusGroup fg sessionList
  void $ addToFocusGroup fg systemList

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

populateObjects :: Widget (HeaderList ObjectPath FormattedText) -> Client -> BusName -> IO ()
populateObjects list bus name = do
  objects <- getObjects bus name
  onList list clearList
  forM_ objects $ \path -> do
    txt <- plainText (T.pack $ formatObjectPath path)
    onList list $ \l -> addToList l path txt

-- Sort alphabetically, but put names starting with ':' last
compareNames :: BusName -> BusName -> Ordering
compareNames name1 name2 = case (formatBusName name1, formatBusName name2) of
  (':':_, ':':_) -> compare name1 name2
  (':':_, _)     -> GT
  (_, ':':_)     -> LT
  _              -> compare name1 name2
