{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick.Main
import Brick.Widgets.Core (Widget)
import qualified Brick.Widgets.Core as Brick
import qualified Brick.AttrMap as Brick
import qualified Brick.Widgets.BusList as Brick
import Graphics.Vty (Event(..), defAttr)

import DBusBrowser.DBus
import qualified DBus.Client as DBus


main :: IO ()
main = do
  (sysBus, sessBus) <- getBusses
  traverse DBus.disconnect sessBus

  sysList <- Brick.mkBusList Brick.SystemBus sysBus

  let app :: App () Event
      app = App {
        appDraw = const [Brick.renderBusList sysList],
        appChooseCursor = neverShowCursor,
        appHandleEvent = \s-> \_ -> halt s,
        appStartEvent = return,
        appAttrMap = const $ Brick.attrMap defAttr [],
        appLiftVtyEvent = id
        }

  defaultMain app ()

ui :: Widget
ui = "Hello, world!"
