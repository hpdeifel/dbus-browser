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
import Data.Monoid

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.AttrMap
import Brick.Renderable
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

renderHeaderList :: HeaderList e -> Bool -> Widget
renderHeaderList hl focus =
  padBottom (Pad 1) (txt (hl^.headerTextL))
  <=>
  (if focus then withFocus else id)
    (renderList (hl^.underlyingListL))

withFocus :: Widget -> Widget
withFocus = updateAttrMap updateMap
  where focusAttr = listSelectedAttr <> "focused"
        updateMap m = applyAttrMappings
                      [ (listSelectedAttr, attrMapLookup focusAttr m) ]
                      m

instance Renderable (HeaderList a) where
  render = renderHeaderList
