{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Brick.Widgets.BusList
       ( BusType(..)
       , BusList(..)
       , getHeaderListL
       , mkBusList
       , renderBusList
       ) where

import Brick.Widgets.HeaderList
import qualified Brick.Widgets.Core as Brick
import qualified Brick.Types as Brick
import Brick.Types (suffixLenses)

import qualified DBus
import qualified DBus.Client as DBus
import DBusBrowser.DBus

import Data.Maybe


data BusType = SystemBus | SessionBus

newtype BusList = BusList {getHeaderList :: HeaderList DBus.BusName}
  deriving (Brick.HandleEvent)

suffixLenses ''BusList

mkBusList :: BusType -> Maybe DBus.Client -> IO BusList
mkBusList typ client = do
  names <- traverse getNames client

  return $ BusList (headerList name title renderItem $ fromMaybe [] names)

  where name = case typ of
          SystemBus  -> "SystemBusList"
          SessionBus -> "SessionBusList"

        title = case typ of
          SystemBus  -> "System Bus"
          SessionBus -> "Session Bus"

        renderItem _ = Brick.str . DBus.formatBusName

renderBusList :: BusList -> Brick.Widget
renderBusList (BusList hl) = renderHeaderList hl
