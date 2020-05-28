import Args
import System.Environment

main = do
  args <- getArgs
  let program = parseArgs args 
  return 0
