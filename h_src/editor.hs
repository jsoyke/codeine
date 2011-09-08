import Control.Applicative
import Control.Exception
import System
import System.IO
import UI.HSCurses.Curses

import Buffer
import Display

data EditWindow =
  EditWindow {
    buffer :: Buffer,
    fileName :: String,
    display :: Display,
    scrollPos :: (Int, Int)
  }

-- Display the EditWindow.
showEditWindow :: EditWindow -> IO ()
showEditWindow editWin = do
  assert (screenY >= 0) return ()
  assert (screenY < h) return ()
  showLines disp (take h $ drop scrollY lines)
  setCursor disp (screenX, screenY) 
  refreshDisplay disp
  where
    disp = display editWin
    buf = buffer editWin
    lines = text $ buffer editWin
    (x, y) = cursorPos buf
    (w, h) = size disp
    (scrollX, scrollY) = scrollPos editWin
    screenY = y - scrollY
    screenX = x - scrollX

-- Get the function that the given key maps to.
-- The returned function can be applied directly to a buffer.
functionForKey :: Key -> Buffer -> Buffer
functionForKey = f
  where
    f KeyUp = moveCursorVert (-1)
    f KeyDown = moveCursorVert 1
    f KeyLeft = moveCursorLeft
    f KeyRight = moveCursorRight
    f KeyEnter = splitLine
    f KeyBackspace = eraseBack
    f (KeyChar '\n') = f KeyEnter
    f (KeyChar '\DEL') = f KeyBackspace
    f (KeyChar '\NAK') = undo
    f (KeyChar '\DC2') = redo
    f (KeyChar c) = insertChar c
    f _ = id

-- If the cursor of the EditWindow is out of range of the currently displayed area,
-- scroll the window so that it remains visible.
scrollToKeepCursorVisible :: EditWindow -> EditWindow
scrollToKeepCursorVisible editWin = editWin { scrollPos = newScrollPos }
  where
    (cursorX, cursorY) = cursorPos $ buffer editWin
    (scrollX, scrollY) = scrollPos editWin
    (w, h) = size $ display editWin
    -- Get a starting position for the provided range, that will include pos.
    newStart (start, end) pos
      | pos < start = pos
      | pos >= end = start + (pos - end) + 1
      | otherwise = start
    xRange = (scrollX, scrollX + w)
    yRange = (scrollY, scrollY + h)
    newScrollPos = (newStart xRange cursorX, newStart yRange cursorY)

-- Perform the action that the given key results in.
-- If the action results in an exit, Nothing is returned.
applyInput :: Key -> EditWindow -> Maybe EditWindow
-- TODO: Either allow for more non-editing key combos?
-- Or consolidate them all into the buffer input?
applyInput (KeyChar '\ETX') _ = Nothing
applyInput key window = Just newWindow
  where
    newBuffer = functionForKey key $ buffer window
    cursor = cursorPos newBuffer
    scroll = scrollPos window
    unscrolled = window { buffer = newBuffer }
    newWindow = scrollToKeepCursorVisible unscrolled

handleEvent :: EditWindow -> Event -> Maybe EditWindow
handleEvent editWin (KeyEvent key) = applyInput key editWin

-- Edit the file. This function will loop until editing is complete.
edit :: Maybe EditWindow -> IO ()
edit Nothing = return ()
edit (Just editWin) = do
  showEditWindow editWin
  event <- waitForEvent $ display editWin
  newWin <- return $ handleEvent editWin event
  edit newWin

-- Read an entire file as a buffer.
readFileAsBuffer :: String -> IO Buffer
readFileAsBuffer fileName = do
  text <- readFile fileName
  return $ bufferWithText text

-- Initialize the display, and start editing a file.
editFile :: String -> IO ()
editFile fileName = do
  initDisplay
  display <- newDisplay
  buf <- readFileAsBuffer fileName
  editWin <- return $ EditWindow {
    buffer = buf,
    fileName = fileName,
    display = display,
    scrollPos = (0, 0)
  }
  edit $ Just editWin
  endDisplay

main = do
  args <- getArgs
  -- Currently only editing one file at a time is supported.
  if length args == 1 then editFile $ head args
  else hPutStrLn stderr "Invalid arguments."
