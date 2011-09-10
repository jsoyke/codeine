module Buffer (
-- types:
  Buffer,
    cursorPos,
    text,
  BufferOptions(BufferOptions),
    autoIndent,
    tabStop,
    useTabs,

-- functions:
  eraseBack,
  eraseForward,
  initBuffer,
  insertChar,
  insertTab,
  insertText,
  moveCursorLeft,
  moveCursorRight,
  moveCursorVert,
  redo,
  setText,
  splitLine,
  undo) where

import Data.Char

data BufferOptions =
  BufferOptions {
    autoIndent :: Bool,

    -- TODO: acknowledge tab options.
    tabStop :: Int,
    useTabs :: Bool
  }

data Buffer =
  Buffer {
    options :: BufferOptions,
    text :: [String],
    cursorPos :: (Int, Int),
    lastEditPos :: (Int, Int),
    lastState :: Maybe Buffer,
    nextState :: Maybe Buffer
  }

-- Create a new buffer containing the provided text.
initBuffer :: BufferOptions -> String -> Buffer
initBuffer options text =
  Buffer {
    options = options,
    text = pad $ lines text,
    cursorPos = (0, 0),
    -- The position of the last edit. Used so the cursor
    -- is set to the position of the last edit on 'redo'.
    lastEditPos = (0, 0),
    lastState = Nothing,
    nextState = Nothing
  }
  where
    pad [] = [""]
    pad lines = lines

-- Undo the last undoable action performed on the buffer.
undo :: Buffer -> Buffer
undo buf = lastBuf
  where
    getLastBuf Nothing = buf
    getLastBuf (Just lastBuf) = lastBuf {
      nextState = Just buf
    }
    lastBuf = getLastBuf $ lastState buf

-- Redo the last action that was undone with 'undo'.
redo :: Buffer -> Buffer
redo buf = nextBuf
  where
    getNextBuf Nothing = buf
    getNextBuf (Just nextBuf) = nextBuf {
      cursorPos = lastEditPos nextBuf
    }
    nextBuf = getNextBuf $ nextState buf

-- Set the text of the buffer.
-- Undoable.
setText :: Buffer -> String -> Buffer
setText buf text = setCursor pos newBuf
  where
    pos = cursorPos buf
    newBuf = buf {
      text = lines text,
      -- TODO: maintain cursor position after this call?
      cursorPos = (0, 0),
      lastEditPos = (0, 0),
      lastState = Just buf,
      nextState = Nothing
    }

-- Inserts a tab.
-- What characters actually get inserted depend on useTabs and tabStop.
-- Undoable.
insertTab :: Buffer -> Buffer
insertTab buf = insertText tab buf
  where
    _useTabs = useTabs $ options buf
    _tabStop = tabStop $ options buf
    tab
      | _useTabs = "\t"
      | otherwise = replicate _tabStop ' '

-- Insert a character at the current cursor position.
-- Undoable.
insertChar :: Char -> Buffer -> Buffer
insertChar c buf = insertText [c] buf

-- Insert text at the current cursor position.
-- Undoable.
insertText :: String -> Buffer -> Buffer
insertText t buf =
  buf {
    text = newText,
    cursorPos = newCursorPos,
    lastEditPos = newCursorPos,
    lastState = Just buf
  }
  where
    (x, y) = cursorPos buf
    newText = [newLine row line | (row, line) <- zip [0..] $ text buf]
    newLine row line
      | row == y = (take x line) ++ t ++ (drop x line)
      | otherwise = line 
    newCursorPos = (x + length t, y)

-- Erase a specified number of characters that after the cursor.
-- Undoable.
eraseForward :: Int -> Buffer -> Buffer
eraseForward 0 buf = buf
eraseForward count buf =
  buf {
    text = newText,
    lastEditPos = cursorPos buf,
    lastState = Just buf
  }
  where
    (x, y) = cursorPos buf
    beg = take y $ text buf
    mid = take x $ text buf !! y
    end = (drop x $ text buf !! y) : (drop (y + 1) $ text buf)
    newEnd = erase count end
    erase _ [] = []
    erase count (h:t)
      | count > length h = erase (count - 1 - length h) t
      | otherwise = (drop count h):t
    newText =
      if length newEnd > 0 then
        beg ++ [mid ++ head newEnd] ++ tail newEnd
      else
        beg ++ [mid]

-- Erase a single character before the cursor.
-- Undoable.
eraseBack :: Buffer -> Buffer
eraseBack buf = eraseForward count newBuf
  where
    oldPos = cursorPos buf
    newBuf = moveCursorLeft buf
    newPos = cursorPos newBuf
    count = if newPos == oldPos then 0 else 1

-- Get the whitespace from the head of a line.
getIndent :: String -> String
getIndent [] = ""
getIndent (h:tail) =
  if isSpace h then h : getIndent tail
  else ""

-- Split the line at the current cursor position.
-- Effectively this inserts a newline.
-- Undoable.
splitLine :: Buffer -> Buffer
splitLine buf =
  buf {
    text = newText,
    cursorPos = newCursorPos,
    lastEditPos = newCursorPos,
    lastState = Just buf
  }
  where
    (x, y) = cursorPos buf
    above = take (y) $ text buf
    below = drop (y+1) $ text buf
    line = text buf !! y
    firstNew = take x line
    indent =
      if autoIndent $ options buf then getIndent firstNew
      else ""
    secondNew = indent ++ drop x line
    newText = above ++ [firstNew, secondNew] ++ below
    newCursorPos = (length indent, y+1)

-- Move the cursor to the specified position.
-- Not undoable.
setCursor :: (Int, Int) -> Buffer -> Buffer
setCursor (x, y) buf = buf { cursorPos = (x, y) }

-- Max Y position possible in the buffer.
maxY :: Buffer -> Int
maxY buf = (length $ text buf) - 1

-- Max X position possible at the given Y position.
maxX :: Buffer -> Int -> Int
maxX buf y = length $ text buf !! y

-- Move the cursor up (negative) or down (positive).
-- Adjusts the X position to something valid.
-- Not undoable.
moveCursorVert :: Int -> Buffer -> Buffer
moveCursorVert dy buf = setCursor (newX, newY) buf
  where
    (x, y) = cursorPos buf
    newY = max 0 $ min (y + dy) $ maxY buf
    newX = max 0 $ min x $ maxX buf newY

-- Move the cursor to the left by one.
-- Moves the cursor up if already at the begining of a line.
-- Not undoable.
moveCursorLeft :: Buffer -> Buffer
moveCursorLeft buf = setCursor (newX, newY) buf
  where
    (x, y) = cursorPos buf
    goUp = x == 0 && y /= 0
    dy = if goUp then -1 else 0
    newY = max 0 $ min (y + dy) $ maxY buf
    newX =
      if goUp then maxX buf newY
      else max 0 $ x - 1

-- Move the cursor to the right by one.
-- Moves the cursor down if already at the end of a line.
-- Not undoable.
moveCursorRight :: Buffer -> Buffer
moveCursorRight buf = setCursor (newX, newY) buf
  where
    (x, y) = cursorPos buf
    goDown = x == (length $ text buf !! y) && y /= maxY buf
    dy = if goDown then 1 else 0
    newY = max 0 $ min (y + dy) $ maxY buf
    newX =
      if goDown then 0
      else min (x + 1) $ maxX buf newY

