{-

the objective of this file is to implement the IncrementReaddir function
purely - that is, get the algorithm working correctly without involving
IO etc.

For now, assume there is only one directory involved - reading /

-}

import Data.List
import Control.Arrow -- first,second

-- test data
currentDirectory = [ ("b", File), ("rdiff-backup-data", Dir) ]
increDirectory   = [ ("a.2010-09-04T12:49:27+01:00.snapshot.gz", File),
	("a.2010-10-04T12:49:27+01:00.snapshot.gz", File),
	("c.2010-10-04T12:49:27+01:00.dir", File),
	("b.2010-09-04T12:49:27+01:00.missing", File) ]
increment = "2010-09-04T12:49:27+01:00"
increment2 = "2010-10-04T12:49:27+01:00"

-- convenience types
data Ftype = File | Dir deriving (Show) -- simplification
type Fpair = (FilePath, Ftype)

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
	dirfiles  = map (second (\_->Dir)) (fetch ".dir")
	fetch s = getBySuffix ('.':incr) $ getBySuffix s increDirectory

-- returns sublist of strings which have the provided suffix,
-- with the suffix removed
getBySuffix :: String -> [Fpair] -> [Fpair]
getBySuffix _ [] = []
getBySuffix suffix fps = 
	map (first (trimSuffix suffix)) $ filter (isSuffixOf suffix . fst) fps
	where
		trimSuffix s = reverse . (drop (length s)) . reverse
