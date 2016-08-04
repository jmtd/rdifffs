{-

    haskell-librsync - an attempt to write a Haskell library to read/write
    /apply rdiff patches etc.

    Presently lives inside the source code for rdifffs and only implements
    the minimum necessary to support that program: namely parsing and applying
    rdiff patches.

    This file: tests for Rdiff.hs

    Copyright © 2016 Jonathan Dowland
    See "LICENSE" for copyright information.
 -}

module Main where

import Rdiff -- runtests
import Data.Either.Utils -- fromRight (missingh)
import Data.Char -- chr
import Text.ParserCombinators.Parsec -- parse, I think

-- test data
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
isRight = not . isLeft

goodpatches = [
      magic ++ "\0"
    , magic ++ (chr 0x02):"hi\0"
    , magic ++ (chr 0x41):"\5hello\0"
    , magic ++ (chr 0x45):"\0\0\0"
    ]
badpatches = [
      "no magic\0"
    , magic -- no null
    , magic ++ "\256not a command\0" 
    , magic ++ [chr 0x41, '\0'] -- missing argument
    , magic ++ [chr 0x45, '\0'] -- missing argument
    , magic ++ (chr 0x45):"\0\0" -- missing argument
    , magic ++ (chr 0x41):"\6\0" -- missing data
    ]

prop_literal str = [LiteralCommand str] == fromRight (p $
    magic ++ [chr 0x41, chr $ length str] ++ str ++ "\0")

p x = parse rdiffPatch "" x

-- real test data!
patch  = "rs\STX6A\GSThu Feb  4 18:18:18 GMT 2010\n\NUL"
output = "Thu Feb  4 18:18:18 GMT 2010\n"
prop_realpatch = applyPatch (fromRight $ p patch) "" == output

runtests = prop_realpatch : (map (isRight . p) goodpatches) ++ (map (isLeft . p) badpatches)

main :: IO ()
main = do
    mapM_ putStrLn $ map show runtests
