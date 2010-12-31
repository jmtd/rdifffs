{-

  grepCopy.hs - a simple tool that will print out filenames supplied
  on the command line if the file is an rdiff patch and contains at
  least one copy instruction

-}

module Main where

import System.Environment -- getArgs
import System.Posix.Files -- fileExist
import Monad -- filterM
import Rdiff

-- is the rdiffCommand a CopyCommand?
isCopyCommand :: RdiffCommand -> Bool
isCopyCommand (CopyCommand _ _) = True
isCopyCommand _ = False

-- does the filepath correspond to an rdiff patch which
containsCopy :: FilePath -> IO Bool
containsCopy fp = do
  e <- fileExist fp
  if e then do
      c <- readFile fp
      case parsePatch c of
        Left _ -> return False
        Right ps -> return $ length (filter isCopyCommand ps) > 0
    else return False

main = do
  args <- getArgs
  fargs <- filterM containsCopy args
  mapM putStrLn fargs
