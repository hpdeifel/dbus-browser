{-# LANGUAGE PackageImports, OverloadedStrings #-}

module Main where

import Graphics.Vty
import "mtl" Control.Monad.State.Lazy
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text, unpack)
import DBusBrowser.DBus
import DBusBrowser.View

data App = App {
  vty :: Vty,
  size :: (Int, Int),
  stack :: ViewStack
}

renderTable :: Table -> Image
renderTable t@(Table p n c) = horiz_cat $ map (renderCol t) [0..maxcol]
  where maxcol = cols c

renderCol :: Table -> Int -> Image
renderCol (Table p n c) cl = vert_cat (map line (reverse p) ++ [selLine c] ++ map line n)
  where line l = text def_attr (col cl l) <|> text def_attr " "
        selLine l = text selectedAttr (col cl l) <|> text selectedAttr " "

selectedAttr = with_fore_color (with_back_color def_attr white) black

renderApp :: App -> Image
renderApp st =  text def_attr (currentTitle (stack st))
            <-> empty_line
            <-> maybe empty_image renderTable' (currentTable . stack $ st)
  where empty_line = string def_attr " "
        renderTable' = translate (0, negate $ viewScroll $ currentView $ stack st) . renderTable
--        renderTable' = renderTable

type Browser = StateT App IO

runBrowser = runStateT

render :: Browser Image
render = do
  (w, h) <- gets size
  modify (doToStack $ modifyView $ scrollView (h-2))
  app <- get
  return $ renderApp app <-> string def_attr " "

doToStack :: (ViewStack -> ViewStack) -> App -> App
doToStack f app = app { stack = f (stack app) }

doToTable :: (Table -> Table) -> App -> App
doToTable = doToStack . modifyTable

selNext :: Browser ()
selNext = modify $ doToTable tableNext

selPrev :: Browser ()
selPrev = modify $ doToTable tablePrev

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
  liftIO $ update (vty st) (pic_for_image pic)
  e <- liftIO $ next_event (vty st)
  case e of
    EvKey (KASCII 'q') [] -> return ()
    EvKey (KASCII 'j') [] -> selNext >> mainloop
    EvKey (KASCII 'k') [] -> selPrev >> mainloop
    EvKey (KASCII 'l') [] -> showNextView >> mainloop
    EvKey (KASCII 'h') [] -> showPrevView >> mainloop
    EvResize w h          -> resize w h >> mainloop
    _ -> mainloop


text attr = string attr . unpack

main = do
  stack <- initStack
  vt <- mkVty
  DisplayRegion w h <- display_bounds $ terminal vt
  runBrowser mainloop $ App vt (fromIntegral w,fromIntegral h) stack
  shutdown vt >> return ()
