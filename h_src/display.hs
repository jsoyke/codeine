module Display (
-- types:
  Display,
    size,
  Event(KeyEvent),
  Key,

-- functions:
  endDisplay,
  initDisplay,
  refreshDisplay,
  setCursor,
  showLines,
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

-- Wait for an event to occur.
waitForEvent :: Display -> IO Event
waitForEvent _ = do
  key <- getCh
  return $ KeyEvent key

-- Show lines on the display.
-- Lines will be truncated if they are too long.
showLines :: Display -> [String] -> IO ()
showLines _ [] = return ()
showLines display lines = doShowLines 0 lines
  where
    (w, h) = size display
    win = window display
    doShowLines row []
      | row < h = do
        wMove win row 0
        wClrToEol win
      | otherwise = return ()
    doShowLines row (line:lines) = do
      mvWAddStr win row 0 (take w line)
      wClrToEol win
      doShowLines (row + 1) lines

-- Set the position of the cursor.
-- This must be called after 'showLines' even if the position hasn't changed.
setCursor :: Display -> (Int, Int) -> IO ()
setCursor display (x, y) =
  wMove (window display) y x

-- Refresh the display.
-- This must be called for any changed to be made visible.
refreshDisplay :: Display -> IO ()
refreshDisplay display = refresh
