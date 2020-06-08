import Args
import System.Environment
import BlockParser
import FromBlock

run :: Program -> IO Int
run prog =
  return 0 

  where
    Program command _ _ _ = prog
    
    blocks :: [String]
    blocks = parseBlocks command

    from_block : _ = blocks

    symbol_map = getFromSymbols prog from_block

main = do
  args <- getArgs
  ret <- run$ parseArgs args 
  return ret
