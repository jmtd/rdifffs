module Main where

import RDiffBackupFS
import System.Directory -- doesDirectoryExist, canonicalizePath, getDirectoryContents
import System.Environment -- getArgs, withArgs
import System.Fuse

main :: IO ()
main = do
  args <- getArgs
  verifyArgs args
  path <- canonicalizePath $ head args
  ensureRdiffBackupDir path
  withArgs (tail args) $ fuseMain (rdiffFSOps path) defaultExceptionHandler
