-- cheapo parsing of patch files

import Data.Char -- chr
import System.Exit -- exitSuccess
import Data.Maybe -- fromJust

patch = "rs\STX6A\GSWed Oct  6 19:40:23 BST 2010\n\NUL"

magic = "rs\STX6"

cmdnames = [ ( 0x41 , "RS_OP_LITERAL_N1" ) , ( 0x0  , "RS_OP_END" ) ]
cmds = [ ( 0x41 , handle_rs_op_literal_n1 ) , ( 0x0  , handle_rs_op_end ) ]

checkheader :: String -> Bool
checkheader header = header == magic

foo :: String -> IO ()
foo p = 
    if checkheader header
        then do foo2 $ drop 4 p
        else error "magic doesn't match"
    where
        header = take 4 p

foo2 :: String -> IO ()
foo2 p = case cmdname of
    Nothing -> error "command expected!"
    Just x  -> do
        putStrLn $ "got command " ++ x
        cmd $ tail p
    where
        cmdbyte = ord (head p)
        cmdname =        lookup cmdbyte cmdnames
        cmd = fromJust $ lookup cmdbyte cmds

handle_rs_op_end :: String -> IO ()
handle_rs_op_end _ = do
    putStrLn "exit op encountered"

handle_rs_op_literal_n1 :: String -> IO ()
handle_rs_op_literal_n1 p = do
    putStrLn $ "inserting: " ++ took
    foo2 rem
    where
        len = ord $ head p
        took = take len (tail p)
        rem = drop (len + 1) p

-- function signature we want:
-- applyRdiff :: ByteString -> ByteString -> ByteString
-- applyRdiff instream patchstream = ...
