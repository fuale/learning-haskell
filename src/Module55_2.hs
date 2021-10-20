module Module55_2 where

import Data.List (isInfixOf)
import System.Directory (getDirectoryContents, removeFile)

main' :: IO ()
main' = do
  putStr "Substring: "
  subString <- getLine
  files <- getDirectoryContents "."
  let toRemove = [file | file <- files, subString `isInfixOf` file]
  if null subString
    then putStrLn "Canceled"
    else mapM_ (\x -> putStr "Removing file: " <* putStrLn x *> removeFile x) toRemove