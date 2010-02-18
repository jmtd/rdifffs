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
cur_files = [".", "..", "b", "rdiff-backup-data" ]
inc_files = [".", "..", "a.2010-02-18T12:06:21Z.snapshot.gz", "b.2010-02-18T12:06:21Z.missing"]

do_readdir = readdir cur_files inc_files incr
-- simulate reading a directory
-- accept [files-in-repo] [files-in-increments-dir]
-- return [files-to-be-displayed]
readdir :: [String] -> [String] -> String -> [String]
readdir curr incr incdate = curr ++ incrshow
    where
        maybeincrtypes = map maybeIncrement incr              -- build possible IncrementRecords
        incrtypes = filter (/= Nothing) maybeincrtypes         -- get definite IncrementRecords
        incrtypes' = map (\x-> case x of Just y -> y) incrtypes -- unpack from the Just wrapper
        relevant = filter ((incdate <=) . irDate) incrtypes'   -- remove irrelevant IncrementRecords
        incrshow = map show relevant                          -- display something
    --filter ((increment <=) . irDate) $ getIncrementRecords files

data IncrementRecord = IncrementRecord {
                           irPath :: FilePath,
                           irDate :: String,
                           irSuff :: String
                       } deriving (Show,Eq)

-- take a filename and possibly return an IncrementRecord
maybeIncrement :: String -> Maybe IncrementRecord
maybeIncrement s = case s =~~ datetime_regex :: Maybe (String, String, String, [String]) of
            (Just (f,_,s,(d:ds))) -> Just IncrementRecord { irPath = f, irDate = d, irSuff = s }
            _ -> Nothing

datetime_regex       = replace "D" "[0-9]" "\\.(DDDD-DD-DDTDD:DD:DDZ)\\."
