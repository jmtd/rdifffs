> module Main where
> 
> import qualified Data.ByteString.Char8 as B
> import Foreign.C.Error
> import System.Posix.Types
> import System.Posix.Files
> import System.Posix.IO
> import System.Fuse
> import System.Environment -- getArgs, withArgs
> import System.Directory -- doesDirectoryExist, canonicalizePath, getDirectoryContents
> import System.FilePath -- pathSeparator, </>, takeFileName
> import Text.Regex.Posix
> import Data.String.Utils -- replace (from libghc6-missingh-dev)
> import System.Posix.Directory
> import Data.List -- isInfixOf, isSuffixOf
> import Data.Maybe -- mapMaybe
> import RdiffFS

The main method is so short I feel it's best to get it out of the way here.

> main :: IO ()
> main = do
>     args <- getArgs
>     verifyArgs args
>     path <- canonicalizePath $ head args
>     ensureRdiffBackupDir path
>     withArgs (tail args) $ fuseMain (rdiffFSOps path) defaultExceptionHandler

> usage :: String
> usage = "archfs3 <rdiff-backup directory> <mountpoint>"
> 
> type RdiffContext = String
> 
> data RdiffBackup = Current String | Increment String deriving (Eq,Show)
> getRdiffBackupDate :: RdiffBackup -> String
> getRdiffBackupDate (Current x) = x
> getRdiffBackupDate (Increment x) = x
> 
> -- we need at least two CMDs: one for us (underlay), one for fuse (mntpoint)
> verifyArgs :: [String] -> IO ()
> verifyArgs xs | length xs > 1 = return ()
> verifyArgs xs | otherwise = error $
>     "invalid number of command-line arguments.\n" ++ "usage: " ++ usage
> 
> isRdiffBackupDir :: FilePath -> IO Bool
> isRdiffBackupDir path = do
>     res <- mapM doesDirectoryExist
>         [path, path </>  "rdiff-backup-data", path </> "rdiff-backup-data" </> "increments" ]
>     (return . and) res
> 
> ensureRdiffBackupDir :: FilePath -> IO ()
> ensureRdiffBackupDir path = do
>     answer <- isRdiffBackupDir path
>     if answer
>         then return ()
>         else error "not a valid rdiff-backup directory"
> 
> datetime_regex       = replace "D" "[0-9]" "\\.(DDDD-DD-DDTDD:DD:DD(Z|[-+]DD:DD))\\."
> current_mirror_regex = "^current_mirror" ++ datetime_regex ++ "data$"
> increment_regex      = "^increments" ++ datetime_regex ++ "dir$"
> 
> getCurrentMirror :: [String] -> RdiffBackup
> getCurrentMirror [] = error "missing current_mirror file"
> getCurrentMirror (x:xs) | x =~ current_mirror_regex = Current $ extractDate x
>                         | otherwise = getCurrentMirror xs
> 
> getIncrements :: [String] -> [RdiffBackup]
> getIncrements files = map (Increment . extractDate) $ filter (=~ increment_regex) files
> 
> extractDate :: String -> String
> extractDate bigstr = head $ matchData (bigstr =~ datetime_regex) where
>         matchData :: (String,String,String,[String]) -> [String]
>         matchData (x,y,z,w) = w
> 
> -- TODO: better name
> getDates :: RdiffContext -> IO [RdiffBackup]
> getDates rdiffCtx = do
>     l <- getDirectoryContents $ rdiffCtx </> "rdiff-backup-data"
>     return $ (getCurrentMirror l) : (getIncrements l)
> 
> type HT = ()
> 
> rdiffFSOps :: RdiffContext -> FuseOperations HT
> rdiffFSOps rdiffCtx = defaultFuseOps { fuseGetFileStat = rdiffGetFileStat rdiffCtx
>                             , fuseOpen        = rdiffOpen rdiffCtx
>                             , fuseRead        = rdiffRead rdiffCtx
>                             , fuseOpenDirectory = rdiffOpenDirectory rdiffCtx
>                             , fuseReadDirectory = rdiffReadDirectory rdiffCtx
>                             , fuseGetFileSystemStats = rdiffGetFileSystemStats
>                             , fuseReadSymbolicLink = rdiffReadSymbolicLink rdiffCtx
>                             }

> buildStat ctx entrytype fsize = FileStat { statEntryType = entrytype
>                          , statFileMode = foldr1 unionFileModes
>                                             [ ownerReadMode
>                                             , ownerExecuteMode
>                                             , groupReadMode
>                                             , groupExecuteMode
>                                             , otherReadMode
>                                             , otherExecuteMode
>                                             ]
>                          , statLinkCount = 2
>                          , statFileOwner = fuseCtxUserID ctx
>                          , statFileGroup = fuseCtxGroupID ctx
>                          , statSpecialDeviceID = 0
>                          , statFileSize = fsize
>                          , statBlocks = 1
>                          , statAccessTime = 0
>                          , statModificationTime = 0
>                          , statStatusChangeTime = 0
>                          }
> dirStat ctx = buildStat ctx Directory 4096
> fileStat ctx = buildStat ctx RegularFile $ fromIntegral $ B.length $ B.pack "test string"
> linkStat ctx = buildStat ctx SymbolicLink$ fromIntegral $ B.length $ B.pack "test string"

Firstly, the top-level FUSE operations. These handle the top-level directory
(list of backup dates, a symlink to the current (most recent) backup); detect
whether the request is for a sub-directory, and dispatch to the appropriate
function (either rdiffCurrent* or rdiffIncrement*) to handle such requests.

> data WhichBackupType = CurrentBackup | IncrementBackup | Neither deriving(Eq)
> whichBackup :: RdiffContext -> String -> IO WhichBackupType
> whichBackup rdiffCtx fpath = do
>     dates <- getDates rdiffCtx
>     if (Current prefix) `elem` dates
>         then return CurrentBackup
>         else if (Increment prefix) `elem` dates
>              then return IncrementBackup
>              else return Neither
>     where
>         (_:path) = fpath
>         prefix = head $ splitDirectories path

rdiffGetFileStat implements getattr(2). We handle requests for the root
directory and the /current symlink within.

> rdiffGetFileStat :: RdiffContext -> FilePath -> IO (Either Errno FileStat)
> rdiffGetFileStat _ "/" = do
>     ctx <- getFuseContext
>     return $ Right $ dirStat ctx
> rdiffGetFileStat _ "/current" = do
>     ctx <- getFuseContext
>     return $ Right $ linkStat ctx
> rdiffGetFileStat rdiffCtx fpath = do
>     which <- whichBackup rdiffCtx fpath
>     case which of
>         CurrentBackup   -> rdiffGetCurrentFileStat rdiffCtx fpath
>         IncrementBackup -> rdiffIncrementGetFileStat rdiffCtx fpath
>         Neither         -> return $ Left eNOENT

> rdiffOpenDirectory :: RdiffContext -> FilePath -> IO Errno
> rdiffOpenDirectory _ "/" = return eOK
> rdiffOpenDirectory rdiffCtx fdir = do
>     which <- whichBackup rdiffCtx fdir
>     case which of
>         CurrentBackup   -> rdiffCurrentOpenDirectory rdiffCtx fdir
>         IncrementBackup -> rdiffIncrementOpenDirectory rdiffCtx fdir
>         Neither         -> return eNOENT

> rdiffReadDirectory :: RdiffContext -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
> rdiffReadDirectory rdiffCtx "/" = do
>     ctx <- getFuseContext
>     dates <- getDates rdiffCtx
>     return $ Right $ (dirs ctx (map getRdiffBackupDate dates)) ++ ([("current", linkStat ctx)])
>     where dirs ctx xs = map (\x -> (x, dirStat ctx)) ([".", ".."] ++ xs)
> rdiffReadDirectory rdiffCtx fdir = do
>     which <- whichBackup rdiffCtx fdir
>     case which of
>         CurrentBackup   -> rdiffCurrentReadDirectory rdiffCtx fdir
>         IncrementBackup -> rdiffIncrementReadDirectory rdiffCtx fdir
>         Neither         -> return $ Left eNOENT

> rdiffOpen :: RdiffContext -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
> rdiffOpen rdiffCtx path mode flags = do
>     which <- whichBackup rdiffCtx path
>     case which of
>         CurrentBackup   -> rdiffCurrentOpen rdiffCtx path mode flags
>         IncrementBackup -> rdiffIncrementOpen rdiffCtx path mode flags
>         Neither         -> return $ Left eNOENT

> rdiffRead :: RdiffContext -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
> rdiffRead rdiffCtx path ht byteCount offset = do
>     which <- whichBackup rdiffCtx path
>     case which of
>         CurrentBackup   -> rdiffCurrentRead   rdiffCtx path ht byteCount offset
>         IncrementBackup -> rdiffIncrementRead rdiffCtx path ht byteCount offset
>         Neither         -> return $ Left eNOENT

> rdiffGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
> rdiffGetFileSystemStats str =
>   return $ Right $ FileSystemStats
>     { fsStatBlockSize = 512
>     , fsStatBlockCount = 1
>     , fsStatBlocksFree = 1
>     , fsStatBlocksAvailable = 1
>     , fsStatFileCount = 5
>     , fsStatFilesFree = 10
>     , fsStatMaxNameLength = 255
>     }

The current implementation of rdiffReadSymbolicLink here assumes that the
list returned by 'getDates' will have the Current backup at the head of
the list. This is true for the current implementation, but it would be nice
to enforce this.

> rdiffReadSymbolicLink :: RdiffContext -> FilePath -> IO (Either Errno FilePath)
> rdiffReadSymbolicLink rdiffCtx "/current" = do
>     dates <- getDates rdiffCtx
>     return $ Right $ getRdiffBackupDate $ head dates
> rdiffReadSymbolicLink rdiffCtx fpath = do
>     which <- whichBackup rdiffCtx fpath
>     case which of
>         CurrentBackup    -> rdiffCurrentReadSymbolicLink rdiffCtx fpath
>         IncrementBackup  -> rdiffIncrementReadSymbolicLink rdiffCtx fpath
>         Neither          -> return $ Left eNOSYS

> ----------------------------------------------------------------------------

Some helper functions for the Current and Increment sets.

> posixToFuseFileType :: [(FileMode, EntryType)]
> posixToFuseFileType = [ (regularFileMode, RegularFile)
>                       , (symbolicLinkMode, SymbolicLink)
>                       , (directoryMode, Directory)
>                       , (namedPipeMode, NamedPipe)
>                       , (characterSpecialMode, CharacterSpecial)
>                       , (blockSpecialMode, BlockSpecial)
>                       , (socketMode, Socket)
>                       ]

> fileNameToFileStat :: FilePath -> IO FileStat
> fileNameToFileStat path = do
>     ctx <- getFuseContext
>     stat <- getSymbolicLinkStatus path
>     let mode = fileMode stat
>     return FileStat { statEntryType = unpick $ lookup (ft mode) posixToFuseFileType 
>                     , statFileMode = mode
>                     , statLinkCount = linkCount stat
>                     , statFileOwner = fuseCtxUserID ctx
>                     , statFileGroup = fuseCtxGroupID ctx
>                     , statSpecialDeviceID = specialDeviceID stat
>                     , statFileSize = fileSize stat
>                     , statBlocks = 1
>                     , statAccessTime = accessTime stat
>                     , statModificationTime = modificationTime stat
>                     , statStatusChangeTime =  statusChangeTime stat
>                     }
>    where
>        unpick :: Maybe EntryType -> EntryType
>        unpick (Just x) = x
>        unpick _ = RegularFile
>        ft mode = mode `intersectFileModes` fileTypeModes

> fileNameToTuple :: FilePath -> IO (String, FileStat)
> fileNameToTuple f = do
>     ctx <- getFuseContext
>     stat <- getSymbolicLinkStatus f
>     return $ case (fileMode stat) `intersectFileModes` fileTypeModes of
>         v | v == symbolicLinkMode -> (f, linkStat ctx)
>           | v == directoryMode    -> (f, dirStat ctx)
>           | v == regularFileMode  -> (f, buildStat ctx RegularFile $ fromIntegral $ (fileSize stat))
>         _   {- default -}         -> (f, fileStat ctx)

Other possibilities are socketMode, characterSpecialMode, blockSpecialMode, namedPipeMode.

Now for the Current-functions. These handle IO requests for stuff under the
current backup tree.

> rdiffGetCurrentFileStat :: RdiffContext -> FilePath -> IO (Either Errno FileStat)
> rdiffGetCurrentFileStat rdiffCtx fpath = do
>     fstat <- fileNameToFileStat realPath
>     return $ Right $ fstat
>     where
>         (_:path) = fpath
>         realPath = joinPath $ rdiffCtx:(tail $ splitDirectories path)

> rdiffCurrentReadSymbolicLink :: RdiffContext -> FilePath -> IO (Either Errno FilePath)
> rdiffCurrentReadSymbolicLink rdiffCtx fpath = do
>     target <- readSymbolicLink $ rdiffCtx </> remainder
>     return $ Right $ target
>     where
>         (_:path) = fpath
>         remainder = joinPath $ tail $ splitDirectories path

> rdiffCurrentReadDirectory rdiffCtx fdir = do
>     l <- getDirectoryContents realdir
>     ret <- mapM (fileNameToTuple . (realdir </>)) $ filter (/= "rdiff-backup-data") l
>     return $ Right $ map (\(s,f) -> (takeFileName s, f)) ret
>     where (_:dir) = fdir
>           remainder = joinPath $ tail $ splitDirectories dir
>           realdir = rdiffCtx </> remainder

This is a really ugly function. We need to call readdir(2) on the underlying
directory and try to pass any error on up to our readdir(2) response. Hence
using the Posix library and trying to handle the error. Another approach might
be to just getDirectoryContents, which is in System.Directory and returns some
fairly useful exception types.

> rdiffCurrentOpenDirectory :: RdiffContext -> FilePath -> IO Errno
> rdiffCurrentOpenDirectory rdiffCtx fpath = do
>     catch (toTry $ rdiffCtx </> remainder) handler
>     where (_:path) = fpath
>           remainder = joinPath $ tail $ splitDirectories path
>           toTry :: FilePath -> IO Errno
>           toTry path = do
>               ds <- openDirStream path
>               closeDirStream ds
>               return eOK
>           handler :: IOError -> IO Errno
>           handler e = return eACCES

> rdiffCurrentOpen :: RdiffContext -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
> rdiffCurrentOpen rdiffCtx fpath mode flags =
>     case mode of
>         ReadOnly -> do             -- Read Write Execute
>             ok <- fileAccess realpath True False False
>             if ok
>                 then return $ Right ()
>                 else return $ Left eACCES
>         _        ->  return $ Left eACCES
>     where (_:path) = fpath
>           remainder = joinPath $ tail $ splitDirectories path
>           realpath = rdiffCtx </> remainder

> rdiffCurrentRead :: RdiffContext -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
> rdiffCurrentRead rdiffCtx fpath _ byteCount offset = do
>     stuff <- readFile realpath
>     return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) $ B.pack stuff
>     where (_:path) = fpath
>           remainder = joinPath $ tail $ splitDirectories path
>           realpath = rdiffCtx </> remainder

Stub increment functions (for now)

Most of these routines need to take a path /<date>/foo/bar and
split it up into <date> and foo/bar bits

> rSplitPath :: FilePath -> (FilePath, FilePath)
> rSplitPath path = (head split, joinPath $ tail split) where
>    split = splitDirectories path

> rdiffIncrementGetFileStat :: RdiffContext -> FilePath -> IO (Either Errno FileStat)
> rdiffIncrementGetFileStat rdiffCtx fpath = do
>     dates <- getDates rdiffCtx
>     ctx <- getFuseContext
>     if increment `elem` (increments dates)
>         then do
>            files <- getDirectoryContents incdir
>            return $ Left eNOSYS
>         else return $ Left eNOENT
>     where
>         (_:path) = fpath
>         (increment, remainder) = rSplitPath path
>         increments dates = map getRdiffBackupDate $ tail dates
>         incdir = rdiffCtx </> "rdiff-backup-data" </> "increments"
>         realpath = incdir </> remainder
>         incfiles files = foldl (++) [] $ map filterSplitSuffix files

> stripSuffix :: String -> String -> Maybe (String, String)
> stripSuffix suffix instring =
>   if suffix `isSuffixOf` instring
>      then Just $ (take (length instring - length suffix) instring, suffix)
>      else Nothing

filterSplitSuffix takes a filename and returns either [] or [(f,suffix)] where f is
the input 'x' with the suffix stripped out.

> filterSplitSuffix x = mapMaybe (\y -> y x) $ map stripSuffix incrementSuffixes

> rdiffIncrementOpenDirectory :: RdiffContext -> FilePath -> IO Errno
> rdiffIncrementOpenDirectory rdiffCtx fdir
>     | dir == prefix = return eOK
>     | otherwise     = return eNOSYS
>     where (_:dir) = fdir
>           prefix = head $ splitDirectories dir

Read the contents of a directory underneath an increment. We need to look
at files under <root>/rdiff-backup-data/increments/<path> matching
*<increment>.<suffix>. The precise suffix influences the directory listing,
e.g. ".missing" means the filename listed *before* <increment> is not present.

An increment's file tree will look as follows (as far as I understand it)

    * start with the list in <dest>/<path>, as per Current
    * look for files matching
      <dest>/rdiff-backup-data/increments/<path>.<datetime_regex>.<suffix>
      where suffix is from the list incrementSuffixes, below
    * we are interested in the matching part of datetime_regex for all dates
      from most recent backwards to our increment time, in that order.
    * looking at the suffix:
      * .missing, then prune the file
      * .snapshot.gz and .diff.gz affect contents of file
      * .dir means the filename was a dir at this point

> incrementSuffixes = [ ".missing", ".diff.gz", ".dir", ".snapshot.gz" ]
> isIncrementFile :: String -> Bool
> isIncrementFile s = or $ map (\y -> y s) $ map isSuffixOf incrementSuffixes

> rdiffIncrementReadDirectory :: RdiffContext -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
> rdiffIncrementReadDirectory repo fdir = do
>     i <- getDirectoryContents incdir
>     c <- rdiffCurrentReadDirectory repo $ pathSeparator: "current" </> remainder
>     case c of
>         Left e -> return (Left e)
>         Right c' -> do
>           i' <- mapM (\f -> do f' <- fstat f; return (f,f')) (i \\ [".", ".."])
>           (return . Right) $ incrementReadDirectory i' c' inc
>     where (_:dir) = fdir
>           inc = head $ splitDirectories dir
>           remainder = joinPath $ tail $ splitDirectories dir
>           incdir = repo </> "rdiff-backup-data" </> "increments" </> remainder
>           fstat f = fileNameToFileStat (incdir </> f)

TODO: we need to handle a failure from getDirectoryContents (exception?)

This function does all the IO to obtain file lists, then passes the results to
incrementReadDirectory which is a pure function.

> rdiffIncrementReadSymbolicLink :: RdiffContext -> FilePath -> IO (Either Errno FilePath)
> rdiffIncrementReadSymbolicLink rdiffCtx fpath = return $ Left eNOSYS

> rdiffIncrementOpen :: RdiffContext -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
> rdiffIncrementOpen rdiffCtx path mode flags = return $ Left eNOENT

> rdiffIncrementRead :: RdiffContext -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
> rdiffIncrementRead rdiffCtx path _ byteCount offset = return $ Left eNOSYS
