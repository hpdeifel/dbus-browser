{-# LANGUAGE ExistentialQuantification #-}

module Brick.Widgets.Layout where

import Brick.Widgets.Core (Widget, hBox, (<+>))
import Brick.Types
import Brick.Renderable
import Graphics.Vty

data Element = forall a. (Renderable a, HandleEvent a) => Element Name a

instance Renderable Element where
  render (Element _ a) = render a

instance HandleEvent Element where
  handleEvent e (Element name a) = Element name $ handleEvent e a

data HLayout = HLayout [Element] Int

instance HandleEvent HLayout where
  handleEvent (EvKey (KChar '\t') []) hl = cycleFocus hl
  handleEvent e (HLayout elems idx) = HLayout newElemes idx
    where newElemes = map maybeEvent $ zip [0..] elems
          maybeEvent (i, el)
            | i == idx  = handleEvent e el
            | otherwise = el

class Layout a where
  cycleFocus :: a -> a

instance Layout HLayout where
  cycleFocus (HLayout elems idx) = HLayout elems ((idx + 1) `mod` length elems)

-- Be aware, the list ist required to be non-empty
hLayout :: [Element] -> HLayout
hLayout elems = HLayout elems 0

renderHLayout :: HLayout -> Widget
renderHLayout (HLayout elems idx) = hBox before <+> selected <+> hBox after
  where before = map (flip render False) $ take idx elems
        selected = render (elems !! idx) True
        after = map (flip render False) $ drop (idx+1) elems
