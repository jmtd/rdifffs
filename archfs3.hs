-- archfs3 : (eventually) fuse filesystem for rdiff-backups, in Haskell

import System -- getArgs
import System.Directory -- doesDirectoryExist
import System.FilePath -- pathSeparator
import Text.Regex.Posix
import Data.String.Utils -- replace (from libghc6-missingh-dev)

usage :: String
usage = "archfs3 <rdiff-backup directory>"

-- we only want one argument for now
verifyArgs :: [String] -> IO ()
verifyArgs [_] = return ()
verifyArgs _ = error $
    "invalid number of command-line arguments.\n" ++ "usage: " ++ usage

isRdiffBackupDir :: FilePath -> IO Bool
isRdiffBackupDir path = do
        res <- mapM doesDirectoryExist [path, rdiff_backup_data, increments]
        return $ and res
        where
            rdiff_backup_data = path ++ pathSeparator:"rdiff-backup-data"
            increments = rdiff_backup_data ++ pathSeparator:"increments"

ensureRdiffBackupDir :: FilePath -> IO ()
ensureRdiffBackupDir path = do
    answer <- isRdiffBackupDir path
    if answer
        then return ()
        else error "not a valid rdiff-backup directory"

datetime_regex       = replace "D" "[0-9]" "\\.(DDDD-DD-DDTDD:DD:DDZ)\\."
current_mirror_regex = "^current_mirror" ++ datetime_regex ++ "data$"
increment_regex      = "^increments" ++ datetime_regex ++ "dir$"

getCurrentMirror :: [String] -> String
getCurrentMirror [] = error "missing current_mirror file"
getCurrentMirror (x:xs) | x =~ current_mirror_regex = x
                        | otherwise = getCurrentMirror xs

getIncrements :: [String] -> [String]
getIncrements files = filter (=~ increment_regex) files

extractDate :: String -> String
extractDate bigstr = head $ matchData (bigstr =~ datetime_regex) where
        matchData :: (String,String,String,[String]) -> [String]
        matchData (x,y,z,w) = w

main :: IO ()
main = do
        args <- getArgs
        verifyArgs args
        let path = head args
        ensureRdiffBackupDir path
        l <- getDirectoryContents $ path ++ pathSeparator:"rdiff-backup-data"
        let c = getCurrentMirror l
        let increments = getIncrements l
        print $ map extractDate (c:increments)
