> module Main where
> 
> import qualified Data.ByteString.Char8 as B
> import Foreign.C.Error
> import System.Posix.Types
> import System.Posix.Files
> import System.Posix.IO
> import System.IO.Error -- isPermissionError etc.
> import System.Fuse
> import System.Environment -- getArgs, withArgs
> import System.Directory -- doesDirectoryExist, canonicalizePath, getDirectoryContents
> import System.FilePath -- pathSeparator, </>, takeFileName
> import Text.Regex.Posix
> import Data.String.Utils -- replace (from libghc6-missingh-dev)
> import System.Posix.Directory
> import Data.List -- isInfixOf, isSuffixOf
> import Data.Maybe -- mapMaybe
> import Foreign -- .&.
> import Control.Arrow -- first

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
> getDates repo = do
>     l <- getDirectoryContents $ repo </> "rdiff-backup-data"
>     return $ (getCurrentMirror l) : (getIncrements l)
> 
> type HT = ()
> 
> rdiffFSOps :: RdiffContext -> FuseOperations HT
> rdiffFSOps repo = defaultFuseOps { fuseGetFileStat = rdiffGetFileStat repo
>                             , fuseOpen        = rdiffOpen repo
>                             , fuseRead        = rdiffRead repo
>                             , fuseOpenDirectory = rdiffOpenDirectory repo
>                             , fuseReadDirectory = rdiffReadDirectory repo
>                             , fuseGetFileSystemStats = rdiffGetFileSystemStats
>                             , fuseReadSymbolicLink = rdiffReadSymbolicLink repo
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
> whichBackup repo path = do
>     dates <- getDates repo
>     if (Current prefix) `elem` dates
>         then return CurrentBackup
>         else if (Increment prefix) `elem` dates
>              then return IncrementBackup
>              else return Neither
>     where
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
> rdiffGetFileStat repo fpath = do
>     which <- whichBackup repo path
>     case which of
>         CurrentBackup   -> rdiffCurrentGetFileStat repo path
>         IncrementBackup -> rdiffIncrementGetFileStat repo path
>         Neither         -> return $ Left eNOENT
>     where
>         (_:path) = fpath

> rdiffOpenDirectory :: RdiffContext -> FilePath -> IO Errno
> rdiffOpenDirectory _ "/" = return eOK
> rdiffOpenDirectory repo fdir = do
>     which <- whichBackup repo dir
>     case which of
>         CurrentBackup   -> rdiffCurrentOpenDirectory repo dir
>         IncrementBackup -> rdiffIncrementOpenDirectory repo dir
>         Neither         -> return eNOENT
>     where
>         (_:dir) = fdir

> type Fpair = (FilePath, FileStat)

> rdiffReadDirectory :: RdiffContext -> FilePath -> IO (Either Errno [Fpair])
> rdiffReadDirectory repo "/" = do
>     ctx <- getFuseContext
>     dates <- getDates repo
>     return $ Right $ (dirs ctx (map getRdiffBackupDate dates)) ++ ([("current", linkStat ctx)])
>     where dirs ctx xs = map (\x -> (x, dirStat ctx)) ([".", ".."] ++ xs)
> rdiffReadDirectory repo fdir = do
>     which <- whichBackup repo dir
>     case which of
>         CurrentBackup   -> rdiffCurrentReadDirectory repo dir
>         IncrementBackup -> rdiffIncrementReadDirectory repo dir
>         Neither         -> return $ Left eNOENT
>     where
>         (_:dir) = fdir

> rdiffOpen :: RdiffContext -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
> rdiffOpen repo fpath mode flags = do
>     which <- whichBackup repo path
>     case which of
>         CurrentBackup   -> rdiffCurrentOpen repo path mode flags
>         IncrementBackup -> rdiffIncrementOpen repo path mode flags
>         Neither         -> return $ Left eNOENT
>     where
>         (_:path) = fpath

> rdiffRead :: RdiffContext -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
> rdiffRead repo fpath ht byteCount offset = do
>     which <- whichBackup repo path
>     case which of
>         CurrentBackup   -> rdiffCurrentRead   repo path ht byteCount offset
>         IncrementBackup -> rdiffIncrementRead repo path ht byteCount offset
>         Neither         -> return $ Left eNOENT
>     where
>         (_:path) = fpath

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
> rdiffReadSymbolicLink repo "/current" = do
>     dates <- getDates repo
>     return $ Right $ getRdiffBackupDate $ head dates
> rdiffReadSymbolicLink repo fpath = do
>     which <- whichBackup repo path
>     case which of
>         CurrentBackup    -> rdiffCurrentReadSymbolicLink repo path
>         IncrementBackup  -> rdiffIncrementReadSymbolicLink repo path
>         Neither          -> return $ Left eNOSYS
>     where
>         (_:path) = fpath

----------------------------------------------------------------------------

Some helper functions for the Current and Increment sets.

> fileNameToFileStat :: FilePath -> IO FileStat
> fileNameToFileStat path = do
>     ctx <- getFuseContext
>     stat <- getSymbolicLinkStatus path
>     let mode = fileMode stat
>     return FileStat { statEntryType = fileModeToEntryType mode
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

> fileNameToTuple :: FilePath -> IO (String, FileStat)
> fileNameToTuple f = do
>     fstat <- fileNameToFileStat f
>     return (f, fstat)

Most of these routines need to take a path /<date>/foo/bar and
split it up into <date> and foo/bar bits. (TODO that the Current
functions still do this themselves)

> rSplitPath :: FilePath -> (FilePath, FilePath)
> rSplitPath path = (head split, joinPath $ tail split) where
>    split = splitDirectories path



----------------------------------------------------------------------------
current functions

These handle IO requests for stuff under the current backup tree.

> rdiffCurrentGetFileStat :: RdiffContext -> FilePath -> IO (Either Errno FileStat)
> rdiffCurrentGetFileStat repo path = do
>     fstat <- fileNameToFileStat realPath
>     return $ Right $ fstat
>     where
>         realPath = joinPath $ repo:(tail $ splitDirectories path)

> rdiffCurrentReadSymbolicLink :: RdiffContext -> FilePath -> IO (Either Errno FilePath)
> rdiffCurrentReadSymbolicLink repo path = do
>     target <- readSymbolicLink $ repo </> remainder
>     return $ Right $ target
>     where
>         remainder = joinPath $ tail $ splitDirectories path

> rdiffCurrentReadDirectory :: RdiffContext -> FilePath -> IO (Either Errno [Fpair])
> rdiffCurrentReadDirectory repo dir = do catch try handler where
>     realdir = joinPath $ repo:(tail $ splitDirectories dir)
>     try = do                                            
>         l <- getDirectoryContents realdir
>         ret <- mapM (fileNameToTuple . (realdir </>)) $ filter (/= "rdiff-backup-data") l
>         return $ Right $ map (\(s,f) -> (takeFileName s, f)) ret
>     handler e | isPermissionError e   = return $ Left eACCES
>               | isDoesNotExistError e = return $ Left eNOENT
>               | otherwise             = return $ Left eFAULT

This is a really ugly function. We need to call readdir(2) on the underlying
directory and try to pass any error on up to our readdir(2) response. Hence
using the Posix library and trying to handle the error. Another approach might
be to just getDirectoryContents, which is in System.Directory and returns some
fairly useful exception types.

> rdiffCurrentOpenDirectory :: RdiffContext -> FilePath -> IO Errno
> rdiffCurrentOpenDirectory repo dir = do catch try handler where
>     realdir = joinPath $ repo:(tail $ splitDirectories dir)
>     try = do
>         ds <- openDirStream realdir
>         closeDirStream ds
>         return eOK
>     handler e = return eACCES

> rdiffCurrentOpen :: RdiffContext -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
> rdiffCurrentOpen repo path mode flags =
>     case mode of
>         ReadOnly -> do             -- Read Write Execute
>             ok <- fileAccess realpath True False False
>             if ok
>                 then return $ Right ()
>                 else return $ Left eACCES
>         _        ->  return $ Left eACCES
>     where
>           remainder = joinPath $ tail $ splitDirectories path
>           realpath = repo </> remainder

> rdiffCurrentRead :: RdiffContext -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
> rdiffCurrentRead repo path _ byteCount offset = do
>     stuff <- readFile realpath
>     return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) $ B.pack stuff
>     where
>           remainder = joinPath $ tail $ splitDirectories path
>           realpath = repo </> remainder

----------------------------------------------------------------------------
increment helper functions

Ensure that the given path corresponds to a valid increment timestamp

> isValidIncrement :: RdiffContext -> String -> IO Bool
> isValidIncrement repo path = do
>     dates <- getDates repo
>     return (increment `elem` increments dates) where
>         (increment, remainder) = rSplitPath path
>         increments = (map getRdiffBackupDate) . tail

> incrementSuffixes = [ ".missing", ".diff.gz", ".dir", ".snapshot.gz" ]

Given a filename, an increment timestamp, and an increment file, is the
increment file relevant to the filename?

> isRelevantFile :: String -> String -> String -> Bool
> isRelevantFile f inc fs = prefixOK && suffixOK where
>     suffix = drop (length f + length inc + 1) fs
>     prefixOK = (f ++ '.':inc) `isPrefixOf` fs
>     suffixOK = suffix `elem` incrementSuffixes

----------------------------------------------------------------------------
increment functions

Try to do the impure IO stuff in the main function, and encapsulate the core
algorithm in an 'inner' pure function.

> rdiffIncrementGetFileStat :: RdiffContext -> FilePath -> IO (Either Errno FileStat)
> rdiffIncrementGetFileStat repo path = do
>     dates <- getDates repo
>     valid <- isValidIncrement repo path
>     if not valid then return $ Left eNOENT
>         else do
>         files <- getDirectoryContents incdir
>         case incFstat file increment files of
>             Nothing -> rdiffCurrentGetFileStat repo path
>             Just (Left x) -> return $ Left x
>             Just (Right x) -> fileNameToFileStat ( incdir </> x) >>= return . Right
>     where
>         (increment, remainder) = rSplitPath path
>         incbase = repo </> "rdiff-backup-data" </> "increments"
>         incdir  = incbase </> (takeDirectory remainder)
>         file    = head $ replace [""] ["."] [takeFileName remainder]

The inner pure function:

> incFstat :: String -> String -> [String] -> Maybe (Either Errno String)
> incFstat file inc files = case length relevant of
>         1 -> Just $ interpretIncFile file inc (head relevant)
>         0 -> Nothing
>         _ -> Just $ Left eNOSYS -- error FIXME what kind?
>     where
>         relevant = filter (isRelevantFile file inc) files

Given a filename, an increment timestamp, and an increment filename,
return either an Errno (for e.g. no virtual file in this increment)
or a increment filename (to derive stat information from)

> interpretIncFile :: String -> String -> String -> Either Errno String
> interpretIncFile file inc incfile
>     | suffix == ".missing" = Left eNOENT
>     | otherwise = Right incfile
>     where suffix = drop (length file + length inc + 1) incfile

> rdiffIncrementOpenDirectory :: RdiffContext -> FilePath -> IO Errno
> rdiffIncrementOpenDirectory repo dir
>     | dir == prefix = return eOK
>     | otherwise     = return eNOSYS
>     where prefix = head $ splitDirectories dir

Get the directory contents for the relevant increments directory and the
corresponding FileStat information. Pass, along with the readDirectory output
for the equivalent Current directory, to a pure inner function.

> rdiffIncrementReadDirectory :: RdiffContext -> FilePath -> IO (Either Errno [Fpair])
> rdiffIncrementReadDirectory repo dir = do
>     i <- getDirectoryContents incdir
>     c <- rdiffCurrentReadDirectory repo $ "current" </> remainder
>     ctx <- getFuseContext
>     case c of
>         Left e -> return (Left e)
>         Right c' -> do
>           i' <- mapM (\f -> do f' <- fstat f; return (f,f')) (i \\ [".", ".."])
>           (return . Right) $ incrementReadDirectory ctx i' c' inc
>     where inc = head $ splitDirectories dir
>           remainder = joinPath $ tail $ splitDirectories dir
>           incdir = repo </> "rdiff-backup-data" </> "increments" </> remainder
>           fstat f = fileNameToFileStat (incdir </> f)

TODO: we need to handle a failure from getDirectoryContents (exception?)

> defaultDir ctx = buildStat ctx Directory 1024

> incrementReadDirectory :: FuseContext -> [Fpair] -> [Fpair] -> String -> [Fpair]
> incrementReadDirectory ctx increDirectory curDirectory incr = nub' $
>   (incfiles ++  dirfiles ++  difffiles ++  curDirectory) \\\ missfiles where
>   nub' = nubBy pairCmp
>   pairCmp (a,_) (b,_) = a == b
>   (\\\) = deleteFirstsBy pairCmp -- specialised '\\'
>   incfiles  = fetch ".snapshot.gz"
>   missfiles = fetch ".missing"
>   difffiles = fetch ".diff.gz"
>   dirfiles  = map (second (\_->(defaultDir ctx))) (fetch ".dir")
>   fetch s = getBySuffix ('.':incr) $ getBySuffix s increDirectory

returns sublist of strings which have the provided suffix,
with the suffix removed

> getBySuffix :: String -> [Fpair] -> [Fpair]
> getBySuffix _ [] = []
> getBySuffix suffix fps = 
>   map (first (trimSuffix suffix)) $ filter (isSuffixOf suffix . fst) fps
>   where
>       trimSuffix s f = take (length f - length s) f


> rdiffIncrementReadSymbolicLink :: RdiffContext -> FilePath -> IO (Either Errno FilePath)
> rdiffIncrementReadSymbolicLink repo path = return $ Left eNOSYS

> rdiffIncrementOpen :: RdiffContext -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
> rdiffIncrementOpen repo path mode flags = return $ Left eNOENT

> rdiffIncrementRead :: RdiffContext -> FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
> rdiffIncrementRead repo path _ byteCount offset = return $ Left eNOSYS
