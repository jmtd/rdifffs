{-

    haskell-librsync - an attempt to write a Haskell library to read/write
    /apply rdiff patches etc.

    Presently lives inside the source code for rdifffs and only implements
    the minimum necessary to support that program: namely parsing and applying
    rdiff patches.

    Copyright © 2010 Jon Dowland
    See "LICENSE" for copyright information.
 -}

module Rdiff
    (
      RdiffCommand(..)
    , rdiffPatch
    , applyPatch
    , applyPatchFile
    , parsePatch
    , magic -- hmm
    ) where

import Text.ParserCombinators.Parsec
import Data.Char -- chr
import Data.Ix -- range
import Data.Either.Utils -- fromRight (missingh)

 -- rdiff patches always begin with this
magic = "rs\STX6"

{-
   data structures which we parse patches to
   rdiff commands can be classified as one of
       LITERAL  - copy a chunk of data from the patch into the output
       COPY     - copy a chunk of data from the input file to the output
       RESERVED - for future use (i.e., an error if found)
 -}
data RdiffCommand = LiteralCommand String | CopyCommand Int Int | ReservedCommand deriving (Show,Eq)

-- rdiff command byte values (see protocol.h from librsync source)
literalcmds0 = map chr (range (0x01,0x40))  -- RS_OP_LITERAL_n (arity 0)
literalcmds1 = map chr (range (0x41, 0x44)) -- RS_OP_LITERAL_Nx (arity 1; x = 2^n)
copycmds     = map chr (range (0x45, 0x54)) -- RS_OP_COPY_Nx_Ny (y = 2^((n-0x45)%4), x goes 1111,2222,4444,8888)
reservedcmds = map chr (range (0x55, 0xff))

-- parsec instructions
rdiffPatch :: GenParser Char st [RdiffCommand]
rdiffPatch = string magic >> rdiffCommand `manyTill` rdiffEof

rdiffCommand = nullaryLiteralCmd <|> unaryLiteralCmd <|> copyCmd

nullaryLiteralCmd = do
    f <- oneOf literalcmds0
    lump <- count (ord f) anyChar
    return $ LiteralCommand lump

unaryLiteralCmd = do
    cmd  <- oneOf literalcmds1
    arg1 <- anyChar
    lump <- count (ord arg1) anyChar
    return $ LiteralCommand lump

copyCmd = do
    cmd  <- oneOf copycmds
    arg1 <- anyChar
    arg2 <- anyChar
    return $ CopyCommand (ord arg1) (ord arg2)

rdiffEof = char '\0'

-- parse a string into a list of RdiffCommands
parsePatch :: String -> Either ParseError [RdiffCommand]
parsePatch x = parse rdiffPatch "" x

-- apply a list of rdiffcommands to an input string
applyPatch :: [RdiffCommand] -> String -> String
applyPatch cs i = concat $ map (\c-> applyCmd c i) cs where
    applyCmd :: RdiffCommand -> String -> String
    applyCmd (LiteralCommand s) _ = s
    applyCmd (CopyCommand x y) _ = error "not implemented"
    applyCmd (ReservedCommand) _ = error "reserved command"

-- apply rdiff patches from FilePaths
applyPatchFile :: FilePath -> FilePath -> IO (Either ParseError String)
applyPatchFile patch infile = do
    p <- readFile patch
    i <- readFile infile
    case (parse rdiffPatch i p) of
        Left e   -> return $ Left e
        Right pt -> return $ Right $ applyPatch pt i
