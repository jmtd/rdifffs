module RdiffFS
  (
    increment
  , increment2
  , incrementReadDirectory 
  ) where

{-

the objective of this file is to implement the IncrementReaddir function
purely - that is, get the algorithm working correctly without involving
IO etc.

For now, assume there is only one directory involved - reading /

-}

import Data.List
import System.Fuse -- EntryType, FileStat
import System.Posix.Files -- stdFileMode
import Control.Arrow -- first,second

-- default FileStat object, for when we don't care
buildStat entrytype = FileStat { statEntryType = entrytype
 , statFileMode = stdFileMode, statLinkCount = 2,       statFileOwner = 0
 , statFileGroup = 0,          statSpecialDeviceID = 0, statFileSize = 1024
 , statBlocks = 1,             statAccessTime = 0
 , statModificationTime = 0, statStatusChangeTime = 0
 }
defaultDir  = buildStat Directory
defaultFile = buildStat RegularFile

-- convenience types
type Fpair = (FilePath, FileStat)

-- hacks thing to make interactive use of above easier
s :: Fpair -> (FilePath, String)
s = second s' where
    s' y = case (statEntryType y) of
              RegularFile -> "File"
              Directory   -> "Dir"
              _           -> "?"

-- test data
currentDirectory = [ ("b", defaultFile), ("rdiff-backup-data", defaultDir) ]
increDirectory   = [ ("a.2010-09-04T12:49:27+01:00.snapshot.gz", defaultFile),
	("a.2010-10-04T12:49:27+01:00.snapshot.gz", defaultFile),
	("c.2010-10-04T12:49:27+01:00.dir", defaultFile),
	("b.2010-09-04T12:49:27+01:00.missing", defaultFile) ]
increment = "2010-09-04T12:49:27+01:00"
increment2 = "2010-10-04T12:49:27+01:00"


currentReadDirectory :: [Fpair]
currentReadDirectory = filter ((/= "rdiff-backup-data") . fst) currentDirectory

incrementReadDirectory :: String -> [Fpair]
incrementReadDirectory incr = nub' $
	(incfiles ++ dirfiles ++ difffiles ++ currentReadDirectory) \\\ missfiles where
	nub' = nubBy pairCmp
	pairCmp (a,_) (b,_) = a == b
	(\\\) = deleteFirstsBy pairCmp -- specialised '\\'
	incfiles  = fetch ".snapshot.gz"
	missfiles = fetch ".missing"
	difffiles = fetch ".diff.gz"
	dirfiles  = map (second (\_->defaultDir)) (fetch ".dir")
	fetch s = getBySuffix ('.':incr) $ getBySuffix s increDirectory

-- returns sublist of strings which have the provided suffix,
-- with the suffix removed
getBySuffix :: String -> [Fpair] -> [Fpair]
getBySuffix _ [] = []
getBySuffix suffix fps = 
	map (first (trimSuffix suffix)) $ filter (isSuffixOf suffix . fst) fps
	where
		trimSuffix s = reverse . (drop (length s)) . reverse
