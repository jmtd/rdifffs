-- start with hello world
-- then accept a cmd line arg
-- then ensure it's a directory
-- then ensure it's an rdiff-backup directory

import System -- getArgs
import System.Directory -- doesDirectoryExist
import System.FilePath -- pathSeparator
import Data.List -- intercalate

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

childdir :: String -> String -> String
childdir a b = intercalate [pathSeparator] [a,b]

ensureRdiffBackupDir :: FilePath -> IO ()
ensureRdiffBackupDir path = do
        ensureDirectory path "not a directory"
        let p2 = childdir path "rdiff-backup-data"
        ensureDirectory p2 "not a valid rdiff-backup directory"
        let p3 = childdir p2 "increments"
        ensureDirectory p3 "not a valid rdiff-backup directory"

exampleCurrentMirror = "current_mirror.2010-01-18T10:27:31Z.data"

split :: Char String -> [String]
split d s = split_ d s [] where
    split_ _ [] a   = a
    split_ d x:xs a = 

currentMirrorFile :: String -> Bool
currentMirrorFile x =
    if length x == length exampleCurrentMirror &&
       (fst $ break (=='.') x) == "current_mirror" &&
       ".data" == snd (splitAt ((length exampleCurrentMirror) - (length ".data")) x)
    then True
    else False
    

ensureCurrentMirror :: [String] -> IO ()
ensureCurrentMirror [] = error "missing current_mirror file"
ensureCurrentMirror (x:xs) = do
    if currentMirrorFile x
        then return ()
        else ensureCurrentMirror xs

main :: IO ()
main = do
        args <- getArgs
        verifyArgs args
        let path = head args
        ensureRdiffBackupDir path
        l <- getDirectoryContents $ childdir path "rdiff-backup-data"
        ensureCurrentMirror l
        print l

-- then, determine dates of backups therein

-- there's the current backup,
    -- rdiff-backup-data/current_mirror.YYYY-MM-DDTHH:MM:SSZ.data
    -- precicely one
-- and increments
    -- rdiff-backup-data/increments.YYYY-MM-DDTHH:MM:SSZ.dir
    -- zero or more
-- then, print those out

