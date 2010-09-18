module Main where

import System.Environment -- getArgs
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as L

main = do
    args <- getArgs
    if length args /= 3
        then error "wrong arg length"
        else fnargh args

fnargh args = do
    if ".gz" == suffix
      then do
        stuff <- fmap decompress (L.readFile ifs)
        L.putStrLn $ L.take (read count) $ L.drop (read skip) $ stuff
      else do
        stuff <- readFile ifs
        putStrLn $ take (read count) $ drop (read skip) stuff
    where [skip,count,ifs] = args
          suffix = drop (length ifs - length ".gz") ifs
