module Main where

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import System.Fuse
import System.Environment -- getArgs, withArgs
import System.Directory -- doesDirectoryExist, canonicalizePath
import System.FilePath -- pathSeparator, </>
import Text.Regex.Posix
import Data.String.Utils -- replace (from libghc6-missingh-dev)

usage :: String
usage = "archfs3 <rdiff-backup directory> <mountpoint>"

type RdiffContext = String

data RdiffBackup = Current String | Increment String deriving (Eq,Show)
getRdiffBackupDate :: RdiffBackup -> String
getRdiffBackupDate (Current x) = x
getRdiffBackupDate (Increment x) = x

-- we need at least two CMDs: one for us (underlay), one for fuse (mntpoint)
verifyArgs :: [String] -> IO ()
verifyArgs [_] = return ()
verifyArgs xs | length xs > 1 = return ()
verifyArgs xs | otherwise = error $
    "invalid number of command-line arguments.\n" ++ "usage: " ++ usage

isRdiffBackupDir :: FilePath -> IO Bool
isRdiffBackupDir path = do
    res <- mapM doesDirectoryExist
        [path, path </>  "rdiff-backup-data", path </> "rdiff-backup-data" </> "increments" ]
    (return . and) res

ensureRdiffBackupDir :: FilePath -> IO ()
ensureRdiffBackupDir path = do
    answer <- isRdiffBackupDir path
    if answer
        then return ()
        else error "not a valid rdiff-backup directory"

datetime_regex       = replace "D" "[0-9]" "\\.(DDDD-DD-DDTDD:DD:DDZ)\\."
current_mirror_regex = "^current_mirror" ++ datetime_regex ++ "data$"
increment_regex      = "^increments" ++ datetime_regex ++ "dir$"

getCurrentMirror :: [String] -> RdiffBackup
getCurrentMirror [] = error "missing current_mirror file"
getCurrentMirror (x:xs) | x =~ current_mirror_regex = Current $ extractDate x
                        | otherwise = getCurrentMirror xs

getIncrements :: [String] -> [RdiffBackup]
getIncrements files = map (Increment . extractDate) $ filter (=~ increment_regex) files

extractDate :: String -> String
extractDate bigstr = head $ matchData (bigstr =~ datetime_regex) where
        matchData :: (String,String,String,[String]) -> [String]
        matchData (x,y,z,w) = w

-- TODO: better name
getDates :: RdiffContext -> IO [RdiffBackup]
getDates rdiffCtx = do
    l <- getDirectoryContents $ rdiffCtx </> "rdiff-backup-data"
    return $ (getCurrentMirror l) : (getIncrements l)

type HT = ()

rdiffFSOps :: RdiffContext -> FuseOperations HT
rdiffFSOps rdiffCtx = defaultFuseOps { fuseGetFileStat = (rdiffGetFileStat rdiffCtx)
                            , fuseOpen        = rdiffOpen
                            , fuseRead        = rdiffRead 
                            , fuseOpenDirectory = (rdiffOpenDirectory rdiffCtx)
                            , fuseReadDirectory = (rdiffReadDirectory rdiffCtx)
                            , fuseGetFileSystemStats = rdiffGetFileSystemStats
                            }
rdiffString :: B.ByteString
rdiffString = B.pack "Hello World, HFuse!\n"

rdiffPath :: FilePath
rdiffPath = "/rdiff"
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = fromIntegral $ B.length rdiffString
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }

rdiffGetFileStat :: RdiffContext -> FilePath -> IO (Either Errno FileStat)
rdiffGetFileStat _ "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx
rdiffGetFileStat _ path | path == rdiffPath = do
    ctx <- getFuseContext
    return $ Right $ fileStat ctx
rdiffGetFileStat rdiffCtx fpath = do
    ctx <- getFuseContext
    dates <- getDates rdiffCtx
    if prefix `elem` (map getRdiffBackupDate dates)
        then rdiffGetCurrentFileStat rdiffCtx fpath
        else return $ Left eNOENT
    where
        (_:path) = fpath
        prefix = head $ splitDirectories path

rdiffGetCurrentFileStat :: RdiffContext -> FilePath -> IO (Either Errno FileStat)
rdiffGetCurrentFileStat rdiffCtx fpath = do
    ctx <- getFuseContext
    tuple <- fileNameToTuple realPath
    return $ Right $ snd tuple
    where
        (_:path) = fpath
        prefix = head $ splitDirectories path
        realPath = joinPath $ rdiffCtx:(tail $ splitDirectories path)

rdiffOpenDirectory :: RdiffContext -> FilePath -> IO Errno
rdiffOpenDirectory _ "/" = return eOK
rdiffOpenDirectory rdiffCtx fdir = do
    dates <- getDates rdiffCtx
    if prefix `elem` (map getRdiffBackupDate dates)
        then return eOK
        else return eNOENT
    where (_:dir) = fdir
          prefix = head $ splitDirectories dir

rdiffReadDirectory :: RdiffContext -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
rdiffReadDirectory rdiffCtx "/" = do
    ctx <- getFuseContext
    dates <- getDates rdiffCtx
    return $ Right $ dirs ctx (map getRdiffBackupDate dates)
    where dirs ctx xs = map (\x -> (x, dirStat ctx)) ([".", ".."] ++ xs)
rdiffReadDirectory rdiffCtx fdir = do
    ctx <- getFuseContext
    dates <- getDates rdiffCtx
    if (Current prefix) `elem` dates
        then rdiffReadCurrentDirectory rdiffCtx fdir
        else if (Increment prefix) `elem` dates
             then return $ Right $ dirs ctx ["OMG"]
             else return (Left (eNOENT)) 
    where (_:dir) = fdir
          prefix = head $ splitDirectories dir
          dirs ctx xs = map (\x -> (x, dirStat ctx)) ([".", ".."] ++ xs)

-- fdir might be /<current> or /<current>/subdir...
-- maybe getDirectoryContents rdiffCtx </> <remainder> ?
rdiffReadCurrentDirectory :: RdiffContext -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
rdiffReadCurrentDirectory rdiffCtx fdir = do
    ctx <- getFuseContext
    if length(splitDirectories dir) == 1
        then do
            l <- getDirectoryContents rdiffCtx
            ret <- mapM (fileNameToTuple . (rdiffCtx </>)) $ filter (/= "rdiff-backup-data") l
            return $ Right $ map (\(s,f) -> (takeFileName s, f)) ret
        else
    where (_:dir) = fdir
          prefix = head $ splitDirectories dir
          dirs ctx xs = map (\x -> (x, dirStat ctx)) ([".", ".."] ++ xs)

fileNameToTuple :: FilePath -> IO (String, FileStat)
fileNameToTuple f = do
    ctx <- getFuseContext
    isFile <- doesFileExist f
    return (f, (if isFile then fileStat else dirStat) ctx)

rdiffOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
rdiffOpen path mode flags
    | path == rdiffPath = case mode of
                            ReadOnly -> return (Right ())
                            _        -> return (Left eACCES)
    | otherwise         = return (Left eNOENT)


rdiffRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
rdiffRead path _ byteCount offset
    | path == rdiffPath =
        return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) rdiffString
    | otherwise         = return $ Left eNOENT

rdiffGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
rdiffGetFileSystemStats str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }

main :: IO ()
main = do
    args <- getArgs
    verifyArgs args
    path <- canonicalizePath $ head args
    ensureRdiffBackupDir path
    withArgs (tail args) $ fuseMain (rdiffFSOps path) defaultExceptionHandler
