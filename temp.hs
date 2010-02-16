-- the purpose of this file is to explore building a correct file listing
-- for rdiff-backup increments
import System.FilePath
import System.Directory
import Text.Regex.Posix
import Data.String.Utils
type RdiffContext = String


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
incr = "2010-02-05T11:32:17Z"
incrkiriath = "2010-02-02T22:28:21Z"
datetime_regex       = replace "D" "[0-9]" "\\.(DDDD-DD-DDTDD:DD:DDZ)\\."
