{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick.Main
import Brick.Widgets.Core
import Brick.Types
import Brick.AttrMap
import Brick.Widgets.BusList
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Layout
import Brick.Util (on)
import Graphics.Vty

import Control.Monad
import Data.Monoid

import DBusBrowser.DBus
import qualified DBus.Client as DBus

type State = HLayout

appEvent :: State -> Event -> EventM (Next State)
appEvent st e = case e of
  EvKey (KChar 'q') [] -> halt st
  EvKey KEsc []        -> halt st
  _                    -> continue $ handleEvent e st

attrs :: AttrMap
attrs = attrMap defAttr
  [ (listAttr        , defAttr)
  , (listSelectedAttr, defAttr `withBackColor` brightBlack)
  , (listSelectedAttr <> "focused", black `on` red)]

draw :: State -> [Widget]
draw st = [w]
  where w = renderHLayout st

main :: IO ()
main = do
  (sysBus, sessBus) <- getBusses

  sysList <- mkBusList SystemBus sysBus
  sesList <- mkBusList SessionBus sessBus

  let app :: App State Event
      app = App {
        appDraw = draw,
        appChooseCursor = neverShowCursor,
        appHandleEvent = appEvent,
        appStartEvent = return,
        appAttrMap = const attrs,
        appLiftVtyEvent = id
        }

      layout = hLayout [ Element "SystemBus" sysList
                       , Element "SessionBus" sesList
                       ]

  void $ defaultMain app layout
