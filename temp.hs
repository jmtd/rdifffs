-- the purpose of this file is to explore building a correct file listing
-- for rdiff-backup increments
import System.FilePath
import System.Directory
import Text.Regex.Posix
import Data.String.Utils
type RdiffContext = String

{-
 - This is the directory structure you get after
 - $ date > src/a
 - $ rdiff-backup src dest
 - $ rm src/a
 - $ date > src/b
 - $ rdiff-backup src dest
        b
        rdiff-backup-data
        rdiff-backup-data/extended_attributes.2010-02-18T12:06:51Z.snapshot
        rdiff-backup-data/backup.log
        rdiff-backup-data/session_statistics.2010-02-18T12:06:51Z.data
        rdiff-backup-data/error_log.2010-02-18T12:06:51Z.data
        rdiff-backup-data/extended_attributes.2010-02-18T12:06:21Z.snapshot
        rdiff-backup-data/chars_to_quote
        rdiff-backup-data/mirror_metadata.2010-02-18T12:06:51Z.snapshot.gz
        rdiff-backup-data/increments
        rdiff-backup-data/increments/a.2010-02-18T12:06:21Z.snapshot.gz
        rdiff-backup-data/increments/b.2010-02-18T12:06:21Z.missing
        rdiff-backup-data/mirror_metadata.2010-02-18T12:06:21Z.diff.gz
        rdiff-backup-data/file_statistics.2010-02-18T12:06:21Z.data.gz
        rdiff-backup-data/current_mirror.2010-02-18T12:06:51Z.data
        rdiff-backup-data/session_statistics.2010-02-18T12:06:21Z.data
        rdiff-backup-data/error_log.2010-02-18T12:06:21Z.data
        rdiff-backup-data/increments.2010-02-18T12:06:21Z.dir
        rdiff-backup-data/file_statistics.2010-02-18T12:06:51Z.data.gz
 - so current (2010-02-18T12:06:51Z) should have just b
 - increment 2010-02-18T12:06:21Z should just have a
 -}

incr = "2010-02-18T12:06:21Z"
files = ["a.2010-02-18T12:06:21Z.snapshot.gz", "b.2010-02-18T12:06:21Z.missing"]

-- we want to
-- list the increments directory, ignoring . and .. (done)
-- assume every file inside matches the regex (done)
-- identify those for which the matched region is lexographically equal to or (done)
-- greater than our increment time. (done)

readdir :: RdiffContext -> FilePath -> IO ()
-- FilePath will be /<increment timestamp>/<sub-path>
readdir repo fpath = do
  l <- getIncrementRecords repo
  mapM_ print $ filter ((increment <=) . irDate) l
  where
    (_:path) = fpath
    increment = head $ splitDirectories path
    remainder = joinPath $ tail $ splitDirectories path
    realdir = repo </> remainder
    incdir = repo </> "rdiff-backup-data" </> "increments" </> remainder

data IncrementRecord = IncrementRecord {
                           irPath :: FilePath,
                           irDate :: String,
                           irSuff :: String
                       } deriving (Show)

-- assume we're reading the root and not a subdir
-- return files which match increment regex
getIncrementRecords :: RdiffContext -> IO [IncrementRecord]
getIncrementRecords repo = do
    l <- getDirectoryContents incdir
    let m = map mapfn l
    return $ fnargh m
    where
        incdir = repo </> "rdiff-backup-data" </> "increments"
        mapfn :: FilePath -> Maybe IncrementRecord
        mapfn fp = case fp =~~ datetime_regex :: Maybe (String, String, String, [String]) of
            (Just (f,_,s,(d:ds))) -> Just IncrementRecord { irPath = f, irDate = d, irSuff = s }
            _ -> Nothing
        -- this is bound to be re-implementing something in the Prelude
        fnargh :: [Maybe a] -> [a]
        fnargh [] = []
        fnargh ((Just x):xs) = x:(fnargh xs)
        fnargh (Nothing:xs) = fnargh xs

real = "/home/jon/wd/mine/archfs3/real/dest"
datetime_regex       = replace "D" "[0-9]" "\\.(DDDD-DD-DDTDD:DD:DDZ)\\."
