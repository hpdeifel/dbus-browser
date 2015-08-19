module Brick.Renderable where

import Brick.Widgets.Core

class Renderable a where
  render :: a -- ^ The thing to render
         -> Bool -- ^ Whether it has focus
         -> Widget
