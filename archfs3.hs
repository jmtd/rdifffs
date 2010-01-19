-- start with hello world
-- then accept a cmd line arg
-- then ensure it's a directory
-- then ensure it's an rdiff-backup directory
-- then, determine dates of backups therein
-- there's the current backup,
    -- rdiff-backup-data/current_mirror.YYYY-MM-DDTHH:MM:SSZ.data
    -- precicely one
-- and increments
    -- rdiff-backup-data/increments.YYYY-MM-DDTHH:MM:SSZ.dir
    -- zero or more
-- then, print those out

import System -- getArgs
import System.Directory -- doesDirectoryExist
import System.FilePath -- pathSeparator
import Data.List -- intercalate
import Text.Regex.Posix
import Data.String.Utils -- replace (from libghc6-missingh-dev)

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

datetime_regex       = replace "D" "[0-9]" "\\.(DDDD-DD-DDTDD:DD:DDZ)\\."
current_mirror_regex = "^current_mirror" ++ datetime_regex ++ "data$"
increment_regex      = "^increments" ++ datetime_regex ++ "dir$"

getCurrentMirror :: [String] -> String
getCurrentMirror [] = error "missing current_mirror file"
getCurrentMirror (x:xs) = do
    if currentMirrorFile x
        then x
        else getCurrentMirror xs
        where
            currentMirrorFile :: String -> Bool
            currentMirrorFile x =
                x =~ current_mirror_regex

getIncrements :: [String] -> [String]
getIncrements files = filter (=~ increment_regex) files

extractDate :: String -> String
extractDate bigstr = do
    let result = bigstr =~ datetime_regex
    head $ matchData result where
        matchData :: (String,String,String,[String]) -> [String]
        matchData (x,y,z,w) = w

main :: IO ()
main = do
        args <- getArgs
        verifyArgs args
        let path = head args
        ensureRdiffBackupDir path
        l <- getDirectoryContents $ childdir path "rdiff-backup-data"
        let c = getCurrentMirror l
        let increments = getIncrements l
        print $ map extractDate (c:increments)
