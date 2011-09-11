module Editor (
-- types:
  Editor,

-- functions:
  editFile) where

import Control.Applicative
import Control.Exception
import System
import System.IO
import UI.HSCurses.Curses

import Buffer
import Display

data Editor =
  Editor {
    buffer :: Buffer,
    fileName :: String,
    display :: Display,
    scrollPos :: (Int, Int)
  }

-- Display the Editor.
showEditor :: Editor -> IO ()
showEditor editor = do
  assert (screenY >= 0) return ()
  assert (screenY < h) return ()
  showLines disp (take h $ drop scrollY lines)
  setCursor disp (screenX, screenY) 
  refreshDisplay disp
  where
    disp = display editor
    buf = buffer editor
    lines = text $ buffer editor
    (x, y) = cursorPos buf
    (w, h) = size disp
    (scrollX, scrollY) = scrollPos editor
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
    f (KeyChar '\t') = insertTab
    f (KeyChar '\n') = f KeyEnter
    f (KeyChar '\DEL') = f KeyBackspace
    f (KeyChar '\NAK') = undo
    f (KeyChar '\DC2') = redo
    f (KeyChar c) = insertChar c
    f _ = id

-- If the cursor of the Editor is out of range of the currently displayed area,
-- scroll the editor so that it remains visible.
scrollToKeepCursorVisible :: Editor -> Editor
scrollToKeepCursorVisible editor = editor { scrollPos = newScrollPos }
  where
    (cursorX, cursorY) = cursorPos $ buffer editor
    (scrollX, scrollY) = scrollPos editor
    (w, h) = size $ display editor
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
applyInput :: Key -> Editor -> IO (Maybe Editor)
-- TODO: Either allow for more non-editing key combos?
-- Or consolidate them all into the buffer input?
applyInput (KeyChar '\ETX') _ = return Nothing
applyInput (KeyChar '\SI') editor = do
  saveBuffer editor
  return $ Just editor
applyInput key editor = return $ Just newEditor
  where
    newBuffer = functionForKey key $ buffer editor
    cursor = cursorPos newBuffer
    scroll = scrollPos editor
    unscrolled = editor { buffer = newBuffer }
    newEditor = scrollToKeepCursorVisible unscrolled

handleEvent :: Editor -> Event -> IO (Maybe Editor)
handleEvent editor (KeyEvent key) = applyInput key editor

-- Edit the file. This function will loop until editing is complete.
edit :: Editor -> IO ()
edit editor = do
  showEditor editor
  event <- waitForEvent $ display editor
  newEditor <- handleEvent editor event
  maybe (return ()) edit newEditor

-- Read an entire file as a buffer.
readFileAsBuffer :: String -> IO Buffer
readFileAsBuffer fileName = do
  text <- readFile fileName
  return $ initBuffer opts text
  where
    opts = BufferOptions {
      autoIndent = True,
      useTabs = False,
      tabStop = 2
    }

-- Save the buffer to disk.
saveBuffer :: Editor -> IO ()
saveBuffer editor = writeFile file txt
  where
    file = fileName editor
    txt = concat [ line ++ "\n" | line <- text $ buffer editor ]

-- Initialize the display, and start editing a file.
editFile :: String -> IO ()
editFile fileName = do
  display <- initDisplay
  buf <- readFileAsBuffer fileName
  editor <- return $ Editor {
    buffer = buf,
    fileName = fileName,
    display = display,
    scrollPos = (0, 0)
  }
  edit editor
  endDisplay display

