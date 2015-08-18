{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Brick.Widgets.HeaderList
       ( HeaderList(..)
       , headerTextL
       , underlyingListL
       , headerList
       , renderHeaderList
       )where

import Data.Text (Text)
import Control.Lens ((^.), (&), (%~), to)

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Graphics.Vty (Event(..), Key(..))

data HeaderList e = HL {
  headerText     :: Text,
  underlyingList :: List e
}

suffixLenses ''HeaderList

instance HandleEvent (HeaderList e) where
  handleEvent e hl = hl & underlyingListL %~ case e of
    EvKey KUp []         -> listMoveUp
    EvKey (KChar 'k') [] -> listMoveUp
    EvKey KDown []       -> listMoveDown
    EvKey (KChar 'j') [] -> listMoveDown
    EvKey (KChar 'g') [] -> listMoveTo 0
    EvKey (KChar 'G') [] -> listMoveBottom
    _                    -> id

listMoveBottom :: List e -> List e
listMoveBottom l = l & listMoveTo (l^.listElementsL^.to length^.to (subtract 1))

headerList :: Name -> Text -> (Bool -> e -> Widget) -> [e] -> HeaderList e
headerList name header itemRender content = HL header (list name itemRender content)

renderHeaderList :: HeaderList e -> Widget
renderHeaderList hl =
  padBottom (Pad 1) (txt (hl^.headerTextL))
  <=>
  renderList (hl^.underlyingListL)
