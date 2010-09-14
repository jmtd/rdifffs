
{-

 incfstat2.hs - *another* attempt to implement the incrementFileStat,
 this time working forwards rather than backwards

 -}

import Data.List -- isPrefixOf

-- test data

incrementSuffixes = [ ".missing", ".diff.gz", ".dir", ".snapshot.gz" ]

files = [ "a.2010-02-04T18:18:15Z.diff.gz"
        , "b"
        , "a.2010-02-04T18:18:17Z.diff.gz"
        , "b.2010-02-04T18:18:18Z.missing"
        , "a.2010-02-07T13:04:04Z.diff.gz"
        , "d.2010-02-06T12:28:31Z.missing" ]

file = "b"
inc = "2010-02-04T18:18:18Z"

isRelevantFile :: String -> String -> String -> Bool
isRelevantFile f inc fs = prefixOK && suffixOK where
    suffix = drop (length f + length inc + 1) fs
    prefixOK = (f ++ '.':inc) `isPrefixOf` fs
    suffixOK = suffix `elem` incrementSuffixes

incFstat :: String -> String -> [String] -> Either Bool String
incFstat file inc files = case length relevant of
        1 -> interpretIncFile file inc (head relevant)
        0 -> curFstat file inc files -- no increment file
        _ -> Left False -- error
    where
        relevant = filter (isRelevantFile file inc) files
        
curFstat file _ _ = Right (file ++ " (current)")-- placeholder

interpretIncFile :: String -> String -> String -> Either Bool String
interpretIncFile file inc incfile
    | suffix == ".missing" = Right "missing" -- Left False -- eNOENT
    | otherwise = Right (file ++ "(interpreted)")-- take stat from inc file
    where suffix = drop (length file + length inc + 1) incfile

