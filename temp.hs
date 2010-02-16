-- the purpose of this file is to explore building a correct file listing
-- for rdiff-backup increments
import System.FilePath
import System.Directory
import Text.Regex.Posix
import Data.String.Utils
type RdiffContext = String
readdir :: RdiffContext -> FilePath -> IO ()
-- FilePath will be /<increment timestamp>/<sub-path>
readdir repo fpath = do
  l <- getDirectoryContents realdir
  l2 <- getDirectoryContents incdir
  mapM_ putStrLn $ filter (/= "rdiff-backup-data") l
  mapM_ putStrLn $ l2
  where
    (_:path) = fpath
    prefix = head $ splitDirectories path
    remainder = joinPath $ tail $ splitDirectories path
    realdir = repo </> remainder
    incdir = repo </> "rdiff-backup-data" </> "increments" </> remainder

interestingIncrements :: String -> [FilePath] -> [FilePath]
interestingIncrements _ [] = []
interestingIncrements incr list =
  map getmatch $ condense $ map applyre list
  where
    applyre :: FilePath -> Maybe (String,String,String,[String])
    applyre fp = fp =~~ datetime_regex
    getmatch :: (String,String,String,[String]) -> String
    getmatch (_,_,_,[x]) = x
    condense :: [Maybe a] -> [a]            
    condense [] = []
    condense ((Just x):xs) = x : condense xs
    condense (Nothing:xs) = condense xs

real = "/home/jon/wd/mine/archfs3/real/dest"
incr = "2010-02-05T11:32:17Z"
datetime_regex       = replace "D" "[0-9]" "\\.(DDDD-DD-DDTDD:DD:DDZ)\\."