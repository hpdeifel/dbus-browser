{-# LANGUAGE OverloadedStrings #-}

module DBusBrowser.View 
       ( View(..)
       , ViewStack(..)
       , initStack
       , currentView
       , currentTitle
       , currentTable
       , modifyTable
       , modifyView
       , pushView
       , popView
       , expandView
       , scrollView
       , Table(..)
       , tableNext
       , tablePrev
       , col
       , cols
       ) where

import Data.Text (Text, append, pack)
import qualified Data.Text as Text
import Data.Maybe
import Data.List (intersperse)
import DBusBrowser.DBus

import Debug.Trace

data View = View {
  viewTitle :: Title, 
  viewTable :: (Maybe Table),
  viewScroll :: Int
}

type Title = Text

data ViewStack = ViewStack View [View] -- cannot be empty

initStack :: IO ViewStack
initStack = do
  (system, session) <- getBusses
  let view = View "Available Busses" 
             (tabulize $ map (\(name,bus) -> BusE (BusEntry name bus)) l) 0
      l = catMaybes $ zipWith (fmap . (,)) ["System Bus", "Session Bus"] [system, session]
  return $ ViewStack view []

currentView :: ViewStack -> View
currentView (ViewStack v _) = v

currentTitle = viewTitle . currentView
currentTable = viewTable . currentView

modifyTable :: (Table -> Table) -> ViewStack -> ViewStack
modifyTable f (ViewStack v vs) = ViewStack v' vs
  where v' = View tit (maybe Nothing (Just . f) tab) scr
        tab = viewTable v
        tit = viewTitle v
        scr = viewScroll v

modifyView :: (View -> View) -> ViewStack -> ViewStack
modifyView f (ViewStack v vs) = ViewStack (f v) vs

pushView :: View -> ViewStack -> ViewStack
pushView v (ViewStack c r) = ViewStack v (c:r)

popView :: ViewStack -> ViewStack
popView v@(ViewStack _ []) = v
popView (ViewStack _ (x:xs)) = ViewStack x xs

expandView :: ViewStack -> IO ViewStack
expandView vs@(ViewStack v l) = do
  new <- expandView' v
  return $ maybe vs (flip ViewStack (v:l)) new

expandView' :: View -> IO (Maybe View)
expandView' (View _ Nothing _) = return Nothing
expandView' (View _ (Just tab) _) = do
  new <- expand (current tab)
  case new of
    Nothing -> return Nothing
    Just (t, es) -> return . Just $ View t (tabulize es) 0

lpref :: Maybe Table -> Int
lpref Nothing = 0
lpref (Just tab) = length . previous $ tab

scrollView :: Int -> View -> View
scrollView height (View tit tab scr)
  | scr > lpref tab = View tit tab (lpref tab)
  | lpref tab >= scr + height = View tit tab ((lpref tab) - height + 1)
  | otherwise = View tit tab scr

data Table = Table {
  previous :: [TableEntry],
  next :: [TableEntry],
  current :: TableEntry
}

tabulize :: [TableEntry] -> Maybe Table
tabulize [] = Nothing
tabulize (x:xs) = Just $ Table [] xs x

tableNext :: Table -> Table
tableNext (Table prev [] cur) = Table prev [] cur
tableNext (Table prev (x:xs) cur) = Table (cur:prev) xs x

tablePrev :: Table -> Table
tablePrev (Table [] next cur) = Table [] next cur
tablePrev (Table (x:xs) next cur) = Table xs (cur:next) x

data BusEntry = BusEntry Text Client
data ServiceEntry = ServiceEntry BusEntry BusName
data ObjectEntry = ObjectEntry ServiceEntry ObjectPath
data InterfaceEntry = InterfaceEntry ObjectEntry InterfaceName
data MemberEntry = MethodEntry InterfaceEntry Method
                 | SignalEntry InterfaceEntry Signal
                 | PropertyEntry InterfaceEntry Prop
data TableEntry = BusE BusEntry
                | ServiceE ServiceEntry
                | ObjectE ObjectEntry
                | InterfaceE InterfaceEntry
                | MemberE MemberEntry

cols (BusE _) = 1
cols (ServiceE _) = 1
cols (ObjectE _) = 1
cols (InterfaceE _) = 1
cols (MemberE _) = 3

col col entry
  | col < cols entry && col >= 0 = case entry of
    BusE (BusEntry name _) -> name
    ServiceE (ServiceEntry _ name) -> busNameText name
    ObjectE (ObjectEntry _ path) -> objectPathText path
    InterfaceE (InterfaceEntry _ name) -> interfaceNameText name
    MemberE me -> memberEntryCol col me
  | otherwise = ""

memberEntryCol :: Int -> MemberEntry -> Text

memberEntryCol 0 (MethodEntry _ _) = "Method"
memberEntryCol 0 (SignalEntry _ _) = "Signal"
memberEntryCol 0 (PropertyEntry _ _) = "Property"

memberEntryCol 1 (MethodEntry _ (Method na _ _)) = memberNameText na
memberEntryCol 1 (SignalEntry _ (Signal na _)) = memberNameText na
memberEntryCol 1 (PropertyEntry _ (Prop na _ _ _)) = na

memberEntryCol 2 (MethodEntry _ (Method _ pi po)) = sh pi `append` " -> " `append` sh po
memberEntryCol 2 (SignalEntry _ (Signal _ ps)) = sh ps
memberEntryCol 2 (PropertyEntry _ (Prop _ _ _ v)) = case v of
  Nothing -> ""
  Just x  -> pack $ show x

sh = Text.concat . intersperse ", " . map paramToText

paramToText :: Parameter -> Text
paramToText (Parameter _ s) = signatureText s


mkMemberEntry :: InterfaceEntry -> Iface -> [MemberEntry]
mkMemberEntry e (Iface ms ss ps) = methods ++ signals ++ properties
  where methods = map (MethodEntry e) ms
        signals = map (SignalEntry e) ss
        properties = map (PropertyEntry e) ps

expand :: TableEntry -> IO (Maybe (Title, [TableEntry]))

expand (BusE e@(BusEntry name client)) = do
  names <- getNames client
  return . Just $ (title, map (ServiceE . ServiceEntry e) names)
  where title = "Services on the " `append` name

expand (ServiceE e@(ServiceEntry bus service)) = do
  objects <- getObjects client service
  return . Just $ (title, map (ObjectE . ObjectEntry e) objects)
  where (BusEntry _ client) = bus
        title = "Objects of " `append` (busNameText service)

expand (ObjectE e@(ObjectEntry srv path)) = do
  ifaces <- getInterfaces client service path
  return . Just $ (title, map (InterfaceE . InterfaceEntry e) ifaces)
  where (ServiceEntry (BusEntry _ client) service) = srv
        title = "Interfaces implemented by " `append` (objectPathText path)

expand (InterfaceE e@(InterfaceEntry obj name)) = do
  members <- getMembers client service path name
  return . Just $ (title, map MemberE $ maybe [] (mkMemberEntry e) members)
  where (ObjectEntry (ServiceEntry (BusEntry _ client) service) path) = obj
        title = "Members of " `append` (interfaceNameText name)

expand _ = return Nothing
