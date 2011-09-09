module Display (
-- types:
  Display,
  Event(KeyEvent),
  Key,

-- functions:
  endDisplay,
  initDisplay,
  refreshDisplay,
  setCursor,
  showLines,
  size,
  waitForEvent) where

import System.IO
import UI.HSCurses.Curses

data Display =
  Display {
    size :: (Int, Int),
    window :: Window
  }

data Event =
  KeyEvent Key

-- Initialize display.
initDisplay :: IO Display
initDisplay = do
  initCurses
  raw True
  echo False
  (h, w) <- scrSize
  window <- initScr
  keypad window True
  return $ Display {
    size = (w, h),
    window = window
  }

-- End display, must be called before exiting.
endDisplay :: Display -> IO ()
endDisplay display = do
  wclear $ window display
  endWin

waitForEvent :: Display -> IO Event
waitForEvent _ = do
  key <- getCh
  return $ KeyEvent key

showLines :: Display -> [String] -> IO ()
showLines _ [] = return ()
showLines display lines = doShowLines display 0 lines
  where
    (h, w) = size display
    win = window display
    doShowLines _ _ [] = return ()
    -- TODO: clear lines that don't get redrawn here.
    doShowLines display row (line:lines) = do
      mvWAddStr win row 0 (take w line)
      wClrToEol win
      doShowLines display (row + 1) lines

setCursor :: Display -> (Int, Int) -> IO ()
setCursor display (x, y) =
  wMove (window display) y x

refreshDisplay :: Display -> IO ()
refreshDisplay display = refresh
