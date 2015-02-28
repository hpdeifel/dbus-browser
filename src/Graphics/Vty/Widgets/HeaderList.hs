{-# LANGUAGE OverloadedStrings #-}
module Graphics.Vty.Widgets.HeaderList
       ( HeaderList
       , newHeaderList
       , setHeader
       , onList
       ) where

import Data.Text (Text)

import Graphics.Vty.Widgets.All

data HeaderList a b = HL {
  headerWidget :: Widget FormattedText,
  listWidget   :: Widget (List a b)
}

instance Show (HeaderList a b) where
  show _ = "HeaderList"

newHeaderList :: Show b => Text -> Int -> IO (Widget (HeaderList a b))
newHeaderList title size = do
  header <- plainText title
  list <- newList size
  box <- return header <--> hFill ' ' 1 <--> return list

  wRef <- newWidget (HL header list) $ \w ->
    w { growHorizontal_ = const $ growHorizontal box
      , growVertical_ = const $ growVertical box
      , setCurrentPosition_ = \_ -> setCurrentPosition box
      , getCursorPosition_ = const $ getCursorPosition box
      , render_ = \_ -> render box
      }

  wRef `relayFocusEvents` list
  wRef `relayKeyEvents` list
  return wRef

setHeader :: Widget (HeaderList a b) -> Text -> IO ()
setHeader wref title = getState wref >>= \hl ->
  setText (headerWidget hl) title

onList :: Widget (HeaderList a b) -> (Widget (List a b) -> IO c) -> IO c
onList wref f = getState wref >>= \hl -> f (listWidget hl)
