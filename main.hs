module Main where

import RdiffBackup
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

usage = "archfs3 <rdiff-backup directory> <mountpoint>"

-- we need at least two CMDs: one for us (underlay), one for fuse (mntpoint)
verifyArgs :: [String] -> IO ()
verifyArgs xs | length xs > 1 = return ()
verifyArgs xs | otherwise = error $
                            "invalid number of command-line arguments.\n" ++ "usage: " ++ usage

rdiffFSOps :: RdiffContext -> FuseOperations HT
rdiffFSOps rdiffCtx = defaultFuseOps { fuseGetFileStat        = rdiffGetFileStat rdiffCtx
                                     , fuseOpen               = rdiffOpen rdiffCtx
                                     , fuseRead               = rdiffRead rdiffCtx
                                     , fuseOpenDirectory      = rdiffOpenDirectory rdiffCtx
                                     , fuseReadDirectory      = rdiffReadDirectory rdiffCtx
                                     , fuseGetFileSystemStats = rdiffGetFileSystemStats
                                     , fuseReadSymbolicLink   = rdiffReadSymbolicLink rdiffCtx
                                     }