import System
import System.IO

import Editor

main = do
  args <- getArgs
  -- Currently only editing one file at a time is supported.
  if length args == 1 then editFile $ head args
  else hPutStrLn stderr "Invalid arguments."
