module Main where

import System.Environment -- getArgs
import Codec.Compression.GZip
--import qualified Data.ByteString.Char8 as B -- one bit works, another doesn't
import qualified Data.ByteString.Lazy as B -- vice versa

main = do
    args <- getArgs
    if length args /= 3
        then error "wrong arg length"
        else fnargh args

fnargh args = do
    if ".gz" == suffix
      then do
        stuff <- fmap decompress (B.readFile ifs)
        putStrLn $ B.unpack $ B.take (read count) $ B.drop (read skip) $ stuff
      else do
        stuff <- readFile ifs
        putStrLn $ take (read count) $ drop (read skip) stuff
    where [skip,count,ifs] = args
          suffix = drop (length ifs - length ".gz") ifs
