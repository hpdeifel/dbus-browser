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
import Brick.Renderable

import qualified DBus
import qualified DBus.Client as DBus
import DBusBrowser.DBus

import Data.Maybe
import Data.List


data BusType = SystemBus | SessionBus

newtype BusList = BusList {getHeaderList :: HeaderList DBus.BusName}
  deriving (Brick.HandleEvent)

suffixLenses ''BusList

mkBusList :: BusType -> Maybe DBus.Client -> IO BusList
mkBusList typ client = do
  names <- traverse getNames client

  let entries = sortBy compareNames (fromMaybe [] names)

  let hl = headerList name title renderItem entries

  return $ BusList hl

  where name = case typ of
          SystemBus  -> "SystemBusList"
          SessionBus -> "SessionBusList"

        title = case typ of
          SystemBus  -> "System Bus"
          SessionBus -> "Session Bus"

        renderItem _ = Brick.padRight Brick.Max . Brick.str . DBus.formatBusName

renderBusList :: BusList -> Bool -> Brick.Widget
renderBusList (BusList hl) = renderHeaderList hl

instance Renderable BusList where
  render = renderBusList

-- Sort alphabetically, but put names starting with ':' last
compareNames :: BusName -> BusName -> Ordering
compareNames name1 name2 = case (formatBusName name1, formatBusName name2) of
  (':':_, ':':_) -> compare name1 name2
  (':':_, _)     -> GT
  (_, ':':_)     -> LT
  _              -> compare name1 name2
