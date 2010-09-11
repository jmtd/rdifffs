import Data.List
import Data.Maybe
import Control.Arrow -- first

stripSuffix :: String -> String -> Maybe (String, String)
stripSuffix suffix instring =
    if suffix `isSuffixOf` instring
       then Just $ (take (length instring - length suffix) instring, suffix)
       else Nothing

incrementSuffixes = [ ".missing", ".diff.gz", ".dir", ".snapshot.gz" ]

files = [ "a.2010-02-04T18:18:15Z.diff.gz"
        , "b"
        , "a.2010-02-04T18:18:17Z.diff.gz"
        , "b.2010-02-04T18:18:18Z.missing"
        , "a.2010-02-07T13:04:04Z.diff.gz"
        , "d.2010-02-06T12:28:31Z.missing" ]


-- given a filename e.g. 'a', 'b', we want to return a filestat
-- firstly, which file should we stat?

-- reduce list of files in increment dir to files with an increment suffix
-- [(f-sans-suffix,suffix)]
incfiles = foldl (++) [] $ map filterSplitSuffix files

-- need to reduce to date-relevant ones (and remove date suffix)
inc = "2010-02-04T18:18:18Z" -- test
datefiles = filter ((isSuffixOf inc) . fst)  incfiles
-- drop date suffix
datefiles2 = map (\(a,b) -> (take (length a - length inc -1) a,b)) datefiles
-- better version
datefiles3 = map (first (\a-> take (length a - length inc -1) a)) datefiles
-- debatably better?
datefiles4 = map (first (fst . fromJust . stripSuffix ('.':inc))) datefiles

{- at this point, datefiles{2,3,4} is [(fname, suffix)]. It should be either:
 -   * zero-length => just call the currentStat method
 -   * length 1    => the increment file is relevant,
 -   * other       => wtf. just throw an error.
 -
 - so in the length-1 case, look at the suffix
     * .missing    => enoent
     * otherwise   => fstat the inc file
 -}

-- filterSplitSuffix takes a filename and returns either [] or [(f,suffix)] where f is
-- the input 'x' with the suffix stripped out.
filterSplitSuffix x = mapMaybe (\y -> y x) $ map stripSuffix incrementSuffixes
