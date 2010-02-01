module Main where

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO

import System.Fuse

type HT = ()

main :: IO ()
main = fuseMain rdiffFSOps defaultExceptionHandler

rdiffFSOps :: FuseOperations HT
rdiffFSOps = defaultFuseOps { fuseGetFileStat = rdiffGetFileStat
                            , fuseOpen        = rdiffOpen
                            , fuseRead        = rdiffRead 
                            , fuseOpenDirectory = rdiffOpenDirectory
                            , fuseReadDirectory = rdiffReadDirectory
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

rdiffGetFileStat :: FilePath -> IO (Either Errno FileStat)
rdiffGetFileStat "/" = do
    ctx <- getFuseContext
    return $ Right $ dirStat ctx
rdiffGetFileStat path | path == rdiffPath = do
    ctx <- getFuseContext
    return $ Right $ fileStat ctx
rdiffGetFileStat _ =
    return $ Left eNOENT

rdiffOpenDirectory "/" = return eOK
rdiffOpenDirectory _   = return eNOENT

rdiffReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
rdiffReadDirectory "/" = do
    ctx <- getFuseContext
    return $ Right [(".",          dirStat  ctx)
                   ,("..",         dirStat  ctx)
                   ,(rdiffName,    fileStat ctx)
                   ]
    where (_:rdiffName) = rdiffPath
rdiffReadDirectory _ = return (Left (eNOENT))

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
