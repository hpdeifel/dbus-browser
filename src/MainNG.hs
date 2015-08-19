{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick.Main
import Brick.Widgets.Core
import Brick.Types
import Brick.AttrMap
import Brick.Widgets.BusList
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Util (on)
import Graphics.Vty

import Control.Monad
import Data.Monoid

import DBusBrowser.DBus
import qualified DBus.Client as DBus


appEvent :: BusList -> Event -> EventM (Next BusList)
appEvent st e = case e of
  EvKey (KChar 'q') [] -> halt st
  EvKey KEsc []        -> halt st
  _                    -> continue $ handleEvent e st

attrs :: AttrMap
attrs = attrMap defAttr
  [ (listAttr        , defAttr)
  , (listSelectedAttr, defAttr `withBackColor` brightBlack)
  , (listSelectedAttr <> "focused", black `on` red)]

main :: IO ()
main = do
  (sysBus, sessBus) <- getBusses

  sysList <- mkBusList SystemBus sysBus
  sesList <- mkBusList SessionBus sessBus

  let app :: App BusList Event
      app = App {
        appDraw = \s -> [renderBusList s True <+> vBorder <+> renderBusList sesList False],
        appChooseCursor = neverShowCursor,
        appHandleEvent = appEvent,
        appStartEvent = return,
        appAttrMap = const attrs,
        appLiftVtyEvent = id
        }

  void $ defaultMain app sysList
