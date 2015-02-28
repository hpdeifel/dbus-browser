module Graphics.Vty.Widgets.Vim
       ( VimBindings(..)
       ) where

import Graphics.Vty.Widgets.All
import Graphics.Vty.Widgets.HeaderList
import Graphics.Vty

class VimBindings a where
  addVimBindings :: (Widget a) -> IO ()

instance VimBindings (HeaderList a b) where
  addVimBindings list = do
    list `onKeyPressed` \_ key _ -> case key of
      KChar 'j' -> onList list scrollDown >> return True
      KChar 'k' -> onList list scrollUp >> return True
      KChar 'g' -> onList list scrollToBeginning >> return True
      KChar 'G' -> onList list scrollToEnd >> return True
      _ -> return False

instance VimBindings FocusGroup where
  addVimBindings fg = do
    fg `onKeyPressed` \_ key _ -> case key of
      KChar 'h' -> focusPrevious fg >> return True
      KChar 'l' -> focusNext fg >> return True
      _ -> return False
