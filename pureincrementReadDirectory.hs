{-

the objective of this file is to implement the IncrementReaddir function
purely - that is, get the algorithm working correctly without involving
IO etc.

For now, assume there is only one directory involved - reading /

-}

import Data.List

-- test data
currentDirectory = [ "b", "rdiff-backup-data" ]
increDirectory   = [ "a.2010-09-04T12:49:27+01:00.snapshot.gz",
	"a.2010-10-04T12:49:27+01:00.snapshot.gz",
	"b.2010-09-04T12:49:27+01:00.missing" ]
increment = "2010-09-04T12:49:27+01:00"

currentReadDirectory :: [FilePath]
currentReadDirectory = filter (/= "rdiff-backup-data") currentDirectory

incrementReadDirectory :: String -> [FilePath]
incrementReadDirectory incr = nub
	(incfiles ++ dirfiles ++ difffiles ++ currentReadDirectory) \\ missfiles where
	incfiles  = fetch ".snapshot.gz"
	missfiles = fetch ".missing"
	difffiles = fetch ".diff.gz"
	dirfiles  = fetch ".dir"
	fetch s = hasSuffix ('.':incr) $ hasSuffix s increDirectory

-- returns sublist of strings which have the provided suffix,
-- with the suffix removed
hasSuffix :: String -> [FilePath] -> [FilePath]
hasSuffix _ [] = []
hasSuffix suffix fps = 
	map (trimSuffix suffix) $ filter (isSuffixOf suffix) fps
	where
		trimSuffix s = reverse . (drop (length s)) . reverse
