{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}
module DBusBrowser.DBus
       ( module DBus
       , module DBus.Introspection
       , Client
       , getBusses
       , getNames
       , getObjects
       , getInterfaces
       , Iface(..)
       , Prop(..)
       , getMembers
       ) where

import Prelude hiding (catch)

import DBus.Client hiding (Method)
import DBus hiding (getSessionAddress,Signal)
import DBus.Introspection hiding (signal)

import qualified Data.Set as S
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (sort,find)
import System.Environment
import qualified Data.Text.IO as TIO
import Control.Exception
import Control.Applicative
import Control.Monad.Maybe
import Control.Monad

type MaybeIO = MaybeT IO

maybeExt :: IO a -> MaybeIO a
maybeExt f = MaybeT $ Just `fmap` f `catch` \(_ :: SomeException) -> return Nothing

wrap :: Maybe a -> MaybeIO a
wrap = MaybeT . return

getMachineId :: MaybeIO T.Text
getMachineId = do
  (content :: T.Text) <- maybeExt $ TIO.readFile "/etc/machine-id"
  wrap $ safeHead (T.lines content)
  where safeHead (x:_) = Just x
        safeHead _ = Nothing

getDisplay :: MaybeIO T.Text
getDisplay = do
  disp <- T.pack <$> (maybeExt $ getEnv "DISPLAY")
  MaybeT $ return $ stripPrefices $ stripScreen disp

  where stripPrefices d =  T.stripPrefix ":" d
                       <|> T.stripPrefix "localhost:" d
                       <|> T.stripPrefix "localhost.localdomain:" d
        stripScreen     =  T.takeWhile (/= '.')

getSessionID :: MaybeIO T.Text
getSessionID = do
  machid <- getMachineId
  disp   <- getDisplay
  return $ machid `T.append` "-" `T.append` disp

getSessionAddress :: MaybeIO T.Text
getSessionAddress = do
  sessId <- getSessionID
  home <- maybeExt $ getEnv "HOME"
  let filename = home ++ "/.dbus/session-bus/" ++ (T.unpack sessId)
      prefix = "DBUS_SESSION_BUS_ADDRESS="
  file <- maybeExt $ TIO.readFile filename
  let line = find (T.isPrefixOf prefix) (T.lines file)
  MaybeT $ return $ T.stripPrefix prefix =<< line

connectSessionFancy :: MaybeIO Client
connectSessionFancy = do
  addr <- getSessionAddress >>= (wrap . parseAddress . T.unpack)
  maybeExt $ connect addr

getSessionBus :: MaybeIO Client
getSessionBus = (maybeExt connectSession) `mplus` connectSessionFancy

getBusses :: IO (Maybe Client, Maybe Client)
getBusses = do
  system <- fmap Just connectSystem `catch` \(_ :: ClientError) -> return Nothing
  session <- runMaybeT getSessionBus
  return (system, session)

getNames :: Client -> IO [BusName]
getNames client = do
  res <- call_ client $
         (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus"
         (memberName_ "ListNames") )
         { methodCallDestination = Just "org.freedesktop.DBus" }

  let names = fromMaybe [] $ fromVariant (methodReturnBody res !! 0)

  return $ sort $ map busName_ names

getObjects :: Client -> BusName -> IO [ObjectPath]
getObjects client service = collectObjects client service "/"

introspect :: Client -> BusName -> ObjectPath -> IO (Maybe Object)
introspect client service path = do
  res <- call_ client $
         (methodCall path "org.freedesktop.DBus.Introspectable" "Introspect") {
           methodCallDestination = Just service }

  let xml = fromVariant $ methodReturnBody res !! 0
      object = parseXML path =<< xml

  writeFile "out.log" (maybe "Error" id xml)

  return object

collectObjects :: Client -> BusName -> ObjectPath -> IO [ObjectPath]
collectObjects client service path = do
  res <- introspect client service path
  case res of
    Nothing -> return []
    Just o  -> if null (objectInterfaces o)
               then subObjects (objectChildren o)
               else fmap (path:) $ subObjects (objectChildren o)

  where subObjects objs = fmap concat $ mapM (collectObjects client service . objectPath) objs

getInterfaces :: Client -> BusName -> ObjectPath -> IO [InterfaceName]
getInterfaces client service path = do
  res <- introspect client service path
  case res of
    Nothing -> return []
    Just o  -> return $ map interfaceName $ objectInterfaces o

data Iface = Iface [Method] [Signal] [Prop]

type PropRead = Bool
type PropWrite = Bool
data Prop = Prop {
  propName :: T.Text,
  propType :: Type,
  propRead :: PropRead,
  propWrite :: PropWrite,
  propValue :: Maybe Variant
}

mkIface :: Client -> BusName -> ObjectPath -> InterfaceName -> Interface -> IO Iface
mkIface client service path iface i = fmap (Iface ms ss) ps'
  where ps' = mapM getProp (interfaceProperties i)
        getProp p = fmap (prop2prop p) (getProperty client service path iface
                                         (T.pack $ propertyName p))
        prop2prop p = Prop (T.pack $ propertyName p) (propertyType p)
                      (propertyRead p) (propertyWrite p)
        ms = interfaceMethods i
        ss = interfaceSignals i


getMembers :: Client -> BusName -> ObjectPath -> InterfaceName -> IO (Maybe Iface)
getMembers client service path iface = do
  res <- introspect client service path
  case res of
    Nothing -> return Nothing
    Just o -> do
      let found = find (\i -> interfaceName i == iface) (objectInterfaces o)
      maybe (return Nothing) (fmap Just . mkIface client service path iface) found


getProperty :: Client -> BusName -> ObjectPath -> InterfaceName -> T.Text -> IO (Maybe Variant)
getProperty client service path iface prop = do
  res <- call_ client $ (methodCall path  "org.freedesktop.DBus.Properties" "Get") {
    methodCallDestination = Just service,
    methodCallBody = [toVariant iface, toVariant prop] }

  let (list :: Maybe Variant) = fromVariant (methodReturnBody res !! 0)

  return $ list
