{-# LANGUAGE PackageImports, OverloadedStrings #-}

module Main where

import Graphics.Vty hiding (resize)
import Data.Default
import "mtl" Control.Monad.State.Lazy
import Data.Text.Lazy (Text, unpack)
import DBusBrowser.View

data App = App {
  vty :: Vty,
  size :: (Int, Int),
  stack :: ViewStack
}

renderTable :: Int -> Table -> Image
renderTable off t@(Table _ _ c) = horizCat $ map (renderCol off t) [0..maxcol]
  where maxcol = cols c

renderCol :: Int -> Table -> Int -> Image
renderCol off (Table p n c) cl = vertCat (map line prev ++ [selLine c] ++ map line n)
  where line l = text defAttr (col cl l) <|> text defAttr " "
        selLine l = text selectedAttr (col cl l) <|> text selectedAttr " "
        prev = drop off (reverse p)

selectedAttr :: Attr
selectedAttr = withForeColor (withBackColor defAttr white) black

renderApp :: App -> Image
renderApp st =  text selectedAttr (currentTitle (stack st))
            <-> empty_line
            <-> maybe emptyImage renderTable' (currentTable . stack $ st)
  where empty_line = string defAttr " "
        renderTable' =  renderTable (viewScroll $ currentView $ stack st)

type Browser = StateT App IO

runBrowser :: StateT s m a -> s -> m (a, s)
runBrowser = runStateT

render :: Browser Image
render = do
  (w, h) <- gets size
  modify (doToStack $ modifyView $ scrollView (h-2))
  app <- get
  return $ crop (fromIntegral w) (fromIntegral h) $ renderApp app <-> string defAttr " "

doToStack :: (ViewStack -> ViewStack) -> App -> App
doToStack f app = app { stack = f (stack app) }

doToTable :: (Table -> Table) -> App -> App
doToTable = doToStack . modifyTable

selNext :: Browser ()
selNext = modify $ doToTable tableNext

selPrev :: Browser ()
selPrev = modify $ doToTable tablePrev

pageUp :: Browser ()
pageUp = do
  (w, h) <- gets size
  forM_ [0..(h-3)] $ \_ ->
    selPrev

pageDown :: Browser ()
pageDown = do
  (w, h) <- gets size
  forM_ [0..(h-3)] $ \_ ->
    selNext

showNextView :: Browser ()
showNextView = do
  st <- get
  new <- liftIO $ expandView (stack st)
  put $ st { stack = new }

showPrevView :: Browser ()
showPrevView = modify $ doToStack popView

resize :: Int -> Int -> Browser ()
resize w h = do
  st <- get
  put $ st { size = (w, h) }

mainloop :: Browser ()
mainloop = do
  st <- get
  pic <- render
  liftIO $ update (vty st) (picForImage pic)
  e <- liftIO $ nextEvent (vty st)
  case e of
    EvKey (KChar 'q') [] -> return ()
    EvKey (KChar 'j') [] -> selNext >> mainloop
    EvKey (KChar 'k') [] -> selPrev >> mainloop
    EvKey (KChar 'l') [] -> showNextView >> mainloop
    EvKey (KChar 'h') [] -> showPrevView >> mainloop
    EvKey KPageUp []      -> pageUp >> mainloop
    EvKey KPageDown []    -> pageDown >> mainloop
    EvResize w h          -> resize w h >> mainloop
    _ -> mainloop


-- text :: Attr -> Text -> Image
-- text attr = string attr . unpack

main :: IO ()
main = do
  stack <- initStack
  vt <- mkVty def
  (w, h) <- displayBounds $ outputIface vt
  runBrowser mainloop $ App vt (fromIntegral w,fromIntegral h) stack
  shutdown vt >> return ()
