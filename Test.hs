{-# OPTIONS_GHC -F -pgmF htfpp #-}
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

import Test.Framework
import Data.Either -- isLeft, isRight
import Data.Either.Utils -- fromRight (missingh)
import Data.Char -- chr
import Text.ParserCombinators.Parsec -- parse

import Rdiff

testWrap x = assertBool $ (isRight . p) x

test_goodpatch_empty  = testWrap $ magic ++ "\0"
test_goodpatch_short  = testWrap $ magic ++ (chr 0x02):"hi\0"
test_goodpatch_longer = testWrap $ magic ++ (chr 0x41):"\5hello\0"
test_goodpatch_dunno  = testWrap $ magic ++ (chr 0x45):"\0\0\0"

badWrap x = assertBool $ (isLeft . p) x

test_badpatch_nomagic      = badWrap $ "no magic\0"
test_badpatch_nonull       = badWrap $ magic
test_badpatch_command      = badWrap $ magic ++ "\256not a command\0"
test_badpatch_missing_arg1 = badWrap $ magic ++ [chr 0x41, '\0']
test_badpatch_missing_arg2 = badWrap $ magic ++ [chr 0x45, '\0']
test_badpatch_missing_arg3 = badWrap $ magic ++ (chr 0x45):"\0\0"
test_badpatch_missing_data = badWrap $ magic ++ (chr 0x41):"\6\0"

prop_literal str = [LiteralCommand str] == fromRight (p $
    magic ++ [chr 0x41, chr $ length str] ++ str ++ "\0")

p x = parse rdiffPatch "" x

-- real test data!
patch  = "rs\STX6A\GSThu Feb  4 18:18:18 GMT 2010\n\NUL"
my_output = "Thu Feb  4 18:18:18 GMT 2010\n"

prop_realpatch = applyPatch (fromRight $ p patch) "" == my_output

main = htfMain htf_thisModulesTests
