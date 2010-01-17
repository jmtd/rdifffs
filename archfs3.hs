-- start with hello world
-- then accept a cmd line arg
-- then ensure it's a directory
-- then ensure it's an rdiff-backup directory

import System -- getArgs
import System.Directory -- doesDirectoryExist
import System.FilePath -- pathSeparator

usage :: String
usage = "archfs3 <rdiff-backup directory>"

-- we only want one argument for now
verifyArgs :: [String] -> IO ()
verifyArgs [_] = return ()
verifyArgs _ = error $
    "invalid number of command-line arguments.\n" ++ "usage: " ++ usage

ensureDirectory :: FilePath -> String -> IO ()
ensureDirectory x errstr = do
    b <- (doesDirectoryExist x)
    if b == True
        then return ()
        else error errstr

ensureRdiffBackupDir :: FilePath -> IO ()
ensureRdiffBackupDir path = do
        ensureDirectory path "not a directory"
        let p2 = path ++ pathSeparator:"rdiff-backup-data"
        ensureDirectory p2 "not a valid rdiff-backup directory"
        let p3 = p2 ++ pathSeparator:"increments"
        ensureDirectory p3 "not a valid rdiff-backup directory"

main :: IO ()
main = do
        args <- getArgs
        verifyArgs args
        let path = head args
        ensureRdiffBackupDir path
        putStrLn path

-- then, determine dates of backups therein

-- there's the current backup,
    -- rdiff-backup-data/current_mirror.YYYY-MM-DDTHH:MM:SSZ.data
    -- precicely one
-- and increments
    -- rdiff-backup-data/increments.YYYY-MM-DDTHH:MM:SSZ.dir
    -- zero or more
-- then, print those out

