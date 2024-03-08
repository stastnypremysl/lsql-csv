import System.Environment

import Lsql.Csv.Main
import Lsql.Csv.Lang.Args


helpCalled :: [String] -> Bool
helpCalled ("-h" : _) = True
helpCalled ("--help" : _) = True
helpCalled (_ : rest) = helpCalled rest
helpCalled [] = False

main = do
  args <- getArgs
  if args == [] || helpCalled args then do
    putStr$ "Usage: lsql-csv [OPTIONS] COMMAND\n\
    	    \  -h/--help\t\t\t\tShow this help.\n\
    	    \  -n/--named\t\t\t\tEnables first line naming convension in csv files.\n\
	    \  -dCHAR/--delimiter=CHAR\t\tChanges default primary delimiter. The default value is ','.\n\
	    \  -sCHAR/--secondary-delimiter=CHAR\tChanges default quote char (secondary delimiter). The default value is '\"'.\n\n\
            \  Details can be found in the documentation.\n\
	    \  See: https://github.com/stastnypremysl/lsql-csv/blob/dev/README.md\n"
  else do
    out <- run$ parseArgs args 
    putStr$ out
