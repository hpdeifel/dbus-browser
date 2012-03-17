{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}
module DBusBrowser.DBus 
       ( module DBus.Types
       , Client
       , DBus.Introspection.Method(..)
       , DBus.Introspection.Signal(..)
       , DBus.Introspection.Parameter(..)
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
import DBus.Client.Simple (connectSystem, connectSession)
import DBus.Message hiding (Signal)
import DBus.Types
import DBus.Connection hiding (connect)
import DBus.Introspection
import DBus.Address

import qualified Data.Set as S
import Data.Maybe
import qualified Data.Text as T
import Data.List (sort,find)
import System.IO
import System.Environment
import qualified Data.Text.IO as TIO
import Control.Exception
import Control.Applicative
import Control.Monad.Maybe
import Control.Monad
import "mtl" Control.Monad.Trans

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
  MaybeT $ return $ stripPrefices disp

  where stripPrefices d =  T.stripPrefix ":" d
                       <|> T.stripPrefix "localhost:" d
                       <|> T.stripPrefix "localhost.localdomain:" d

getSessionID :: MaybeIO T.Text
getSessionID = do
  machid <- getMachineId
  disp   <- getDisplay
  return $ machid `T.append` "-" `T.append` disp

getSessionAddress :: MaybeIO T.Text
getSessionAddress = do
  id <- getSessionID
  home <- maybeExt $ getEnv "HOME"
  let filename = home ++ "/.dbus/session-bus/" ++ (T.unpack id)
      prefix = "DBUS_SESSION_BUS_ADDRESS="
  file <- maybeExt $ TIO.readFile filename
  let line = find (T.isPrefixOf prefix) (T.lines file)
  MaybeT $ return $ T.stripPrefix prefix =<< line

connectSessionFancy :: MaybeIO Client
connectSessionFancy = do
  addr <- getSessionAddress >>= (wrap . address)
  maybeExt $ connect addr

getSessionBus :: MaybeIO Client
getSessionBus = (maybeExt connectSession) `mplus` connectSessionFancy

getBusses :: IO (Maybe Client, Maybe Client)
getBusses = do
  system <- fmap Just connectSystem `catch` \(e :: ConnectionError) -> return Nothing
  session <- runMaybeT getSessionBus
  return (system, session)

getNames :: Client -> IO [BusName]
getNames client = do
  res <- call_ client $ MethodCall {
    methodCallPath = "/org/freedesktop/DBus",
    methodCallMember = "ListNames",
    methodCallInterface = Just "org.freedesktop.DBus",
    methodCallDestination = Just "org.freedesktop.DBus",
    methodCallFlags = S.empty,
    methodCallBody = [] }

  let names = fromMaybe [] $ fromVariant (methodReturnBody res !! 0)

  return $ sort $ map busName_ names

getObjects :: Client -> BusName -> IO [ObjectPath]
getObjects client service = collectObjects client service "/"
 
introspect :: Client -> BusName -> ObjectPath -> IO (Maybe Object)
introspect client service path = do
  res <- call_ client $ MethodCall {
    methodCallPath = path,
    methodCallMember = "Introspect",
    methodCallInterface = Just "org.freedesktop.DBus.Introspectable",
    methodCallDestination = Just service,
    methodCallFlags = S.empty,
    methodCallBody = [] }

  let xml = fromVariant $ methodReturnBody res !! 0
      object = fromXML path =<< xml

  return object

collectObjects :: Client -> BusName -> ObjectPath -> IO [ObjectPath]
collectObjects client service path = do
  res <- introspect client service path
  case res of
    Nothing -> return []
    Just (Object _ [] objs) -> subObjects objs
    Just (Object _ iface objs) -> fmap (path:) $ subObjects objs

  where subObjects objs = fmap concat $ mapM (collectObjects client service . getPath) objs  

getPath (Object p _ _) = p

getInterfaces :: Client -> BusName -> ObjectPath -> IO [InterfaceName]
getInterfaces client service path = do
  res <- introspect client service path
  case res of
    Nothing -> return []
    Just (Object _ ifaces _) -> return $ map getIfaceName ifaces

getIfaceName (Interface n _ _ _) = n

data Iface = Iface [Method] [Signal] [Prop]

data Prop = Prop T.Text Signature [PropertyAccess] (Maybe Variant)

mkIface :: Client -> BusName -> ObjectPath -> InterfaceName -> Interface -> IO Iface
mkIface client service path iface (Interface _ ms ss ps) = fmap (Iface ms ss) ps'
  where ps' = mapM getProp ps
        getProp (Property t s ac) = fmap (Prop t s ac) (getProperty client service path iface t)

getMembers :: Client -> BusName -> ObjectPath -> InterfaceName -> IO (Maybe Iface)
getMembers client service path iface = do
  res <- introspect client service path
  case res of 
    Nothing -> return Nothing
    Just (Object _ ifs _) -> do
      let found = find (\(Interface n _ _ _) -> n == iface) ifs
      maybe (return Nothing) (fmap Just . mkIface client service path iface) found


getProperty :: Client -> BusName -> ObjectPath -> InterfaceName -> T.Text -> IO (Maybe Variant)
getProperty client service path iface prop = do
  res <- call_ client $ MethodCall {
    methodCallPath = path,
    methodCallMember = "Get",
    methodCallInterface = Just "org.freedesktop.DBus.Properties",
    methodCallDestination = Just service,
    methodCallFlags = S.empty,
    methodCallBody = [toVariant iface, toVariant prop] }

  let (list :: Maybe Variant) = fromVariant (methodReturnBody res !! 0)

  return $ list
