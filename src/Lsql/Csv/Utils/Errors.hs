module Lsql.Csv.Utils.Errors(
    oneArgOnly, oneArgErr,
    twoArgOnly, twoArgErr

  ) where

oneArgOnly :: [a] -> Bool
oneArgOnly (_ : _ : _) = False
oneArgOnly _ = True

oneArgErr :: String -> String
oneArgErr w = w ++ " can have only one argument."

twoArgOnly :: [a] -> Bool
twoArgOnly (_ : _ : _ : _) = False
twoArgOnly (_ : _ : _) = True
twoArgOnly _  = False

twoArgErr :: String -> String
twoArgErr w = w ++ " can have two argument only."
