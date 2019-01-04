-- |
-- Module      :  Path.IO
-- Copyright   :  © 2016–2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides an interface to "System.Directory" for users of the
-- "Path" module. It also implements commonly used primitives like recursive
-- scanning and copying of directories, working with temporary
-- files\/directories, etc.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Path.IO
  ( -- * Actions on directories
    createDir
  , createDirIfMissing
  , ensureDir
  , removeDir
  , removeDirRecur
  , renameDir
  , listDir
  , listDirRel
  , listDirRecur
  , listDirRecurRel
  , copyDirRecur
  , copyDirRecur'
    -- ** Walking directory trees
  , WalkAction (..)
  , walkDir
  , walkDirRel
  , walkDirAccum
  , walkDirAccumRel
    -- ** Current working directory
  , getCurrentDir
  , setCurrentDir
  , withCurrentDir
    -- * Pre-defined directories
  , getHomeDir
  , getAppUserDataDir
  , getUserDocsDir
  , getTempDir
#if MIN_VERSION_directory(1,2,3)
  , XdgDirectory (..)
  , getXdgDir
#endif
    -- * Path transformation
  , AnyPath (..)
  , resolveFile
  , resolveFile'
  , resolveDir
  , resolveDir'
    -- * Actions on files
  , removeFile
  , renameFile
  , copyFile
  , findExecutable
  , findFile
  , findFiles
  , findFilesWith
    -- * Symbolic links
  , isSymlink
    -- * Temporary files and directories
  , withTempFile
  , withTempDir
  , withSystemTempFile
  , withSystemTempDir
  , openTempFile
  , openBinaryTempFile
  , createTempDir
    -- * Existence tests
  , doesFileExist
  , doesDirExist
  , isLocationOccupied
  , forgivingAbsence
  , ignoringAbsence
    -- * Permissions
  , D.Permissions
  , D.emptyPermissions
  , D.readable
  , D.writable
  , D.executable
  , D.searchable
  , D.setOwnerReadable
  , D.setOwnerWritable
  , D.setOwnerExecutable
  , D.setOwnerSearchable
  , getPermissions
  , setPermissions
  , copyPermissions
    -- * Timestamps
#if MIN_VERSION_directory(1,2,3)
  , getAccessTime
  , setAccessTime
  , setModificationTime
#endif
  , getModificationTime )
where

import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Writer.Lazy (WriterT, execWriterT, tell)
import Data.Either (lefts, rights)
import Data.List ((\\))
import Data.Time (UTCTime)
import Path
import System.IO (Handle)
import System.IO.Error (isDoesNotExistError)
import qualified Data.DList               as DList
import qualified Data.Set                 as S
import qualified System.Directory         as D
import qualified System.FilePath          as F
import qualified System.IO.Temp           as T
import qualified System.PosixCompat.Files as P

#if MIN_VERSION_directory(1,2,3)
import System.Directory (XdgDirectory)
#endif

----------------------------------------------------------------------------
-- Actions on directories

-- | @'createDir' dir@ creates a new directory @dir@ which is initially
-- empty, or as near to empty as the operating system allows.
--
-- The operation may fail with:
--
-- * 'isPermissionError' \/ 'PermissionDenied'
-- The process has insufficient privileges to perform the operation.
-- @[EROFS, EACCES]@
--
-- * 'isAlreadyExistsError' \/ 'AlreadyExists'
-- The operand refers to a directory that already exists.
-- @ [EEXIST]@
--
-- * 'HardwareFault'
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * 'InvalidArgument'
-- The operand is not a valid directory name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'NoSuchThing'
-- There is no path to the directory.
-- @[ENOENT, ENOTDIR]@
--
-- * 'ResourceExhausted' Insufficient resources (virtual memory, process
-- file descriptors, physical disk space, etc.) are available to perform the
-- operation. @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
--
-- * 'InappropriateType'
-- The path refers to an existing non-directory object.
-- @[EEXIST]@

createDir :: MonadIO m => Path b Dir -> m ()
createDir = liftD D.createDirectory

-- | @'createDirIfMissing' parents dir@ creates a new directory @dir@ if it
-- doesn't exist. If the first argument is 'True' the function will also
-- create all parent directories if they are missing.

createDirIfMissing :: MonadIO m
  => Bool              -- ^ Create its parents too?
  -> Path b Dir        -- ^ The path to the directory you want to make
  -> m ()
createDirIfMissing p = liftD (D.createDirectoryIfMissing p)

-- | Ensure that a directory exists creating it and its parent directories
-- if necessary. This is just a handy shortcut:
--
-- > ensureDir = createDirIfMissing True
--
-- @since 0.3.1

ensureDir :: MonadIO m => Path b Dir -> m ()
ensureDir = createDirIfMissing True

-- | @'removeDir' dir@ removes an existing directory @dir@. The
-- implementation may specify additional constraints which must be satisfied
-- before a directory can be removed (e.g. the directory has to be empty, or
-- may not be in use by other processes). It is not legal for an
-- implementation to partially remove a directory unless the entire
-- directory is removed. A conformant implementation need not support
-- directory removal in all situations (e.g. removal of the root directory).
--
-- The operation may fail with:
--
-- * 'HardwareFault'
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * 'InvalidArgument'
-- The operand is not a valid directory name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError' \/ 'NoSuchThing'
-- The directory does not exist.
-- @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError' \/ 'PermissionDenied'
-- The process has insufficient privileges to perform the operation.
-- @[EROFS, EACCES, EPERM]@
--
-- * 'UnsatisfiedConstraints'
-- Implementation-dependent constraints are not satisfied.
-- @[EBUSY, ENOTEMPTY, EEXIST]@
--
-- * 'UnsupportedOperation'
-- The implementation does not support removal in this situation.
-- @[EINVAL]@
--
-- * 'InappropriateType'
-- The operand refers to an existing non-directory object.
-- @[ENOTDIR]@

removeDir :: MonadIO m => Path b Dir -> m ()
removeDir = liftD D.removeDirectory

-- | @'removeDirRecur' dir@ removes an existing directory @dir@ together
-- with its contents and sub-directories. Within this directory, symbolic
-- links are removed without affecting their targets.

removeDirRecur :: MonadIO m => Path b Dir -> m ()
removeDirRecur = liftD D.removeDirectoryRecursive

-- |@'renameDir' old new@ changes the name of an existing directory from
-- @old@ to @new@. If the @new@ directory already exists, it is atomically
-- replaced by the @old@ directory. If the @new@ directory is neither the
-- @old@ directory nor an alias of the @old@ directory, it is removed as if
-- by 'removeDir'. A conformant implementation need not support renaming
-- directories in all situations (e.g. renaming to an existing directory, or
-- across different physical devices), but the constraints must be
-- documented.
--
-- On Win32 platforms, @renameDir@ fails if the @new@ directory already
-- exists.
--
-- The operation may fail with:
--
-- * 'HardwareFault'
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * 'InvalidArgument'
-- Either operand is not a valid directory name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError' \/ 'NoSuchThing'
-- The original directory does not exist, or there is no path to the target.
-- @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError' \/ 'PermissionDenied'
-- The process has insufficient privileges to perform the operation.
-- @[EROFS, EACCES, EPERM]@
--
-- * 'ResourceExhausted'
-- Insufficient resources are available to perform the operation.
-- @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
--
-- * 'UnsatisfiedConstraints'
-- Implementation-dependent constraints are not satisfied.
-- @[EBUSY, ENOTEMPTY, EEXIST]@
--
-- * 'UnsupportedOperation'
-- The implementation does not support renaming in this situation.
-- @[EINVAL, EXDEV]@
--
-- * 'InappropriateType'
-- Either path refers to an existing non-directory object.
-- @[ENOTDIR, EISDIR]@

renameDir :: MonadIO m
  => Path b0 Dir       -- ^ Old name
  -> Path b1 Dir       -- ^ New name
  -> m ()
renameDir = liftD2 D.renameDirectory

-- | @'listDir' dir@ returns a list of /all/ entries in @dir@ without the
-- special entries (@.@ and @..@). Entries are not sorted.
--
-- The operation may fail with:
--
-- * 'HardwareFault'
--   A physical I\/O error has occurred.
--   @[EIO]@
--
-- * 'InvalidArgument'
--   The operand is not a valid directory name.
--   @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError' \/ 'NoSuchThing'
--   The directory does not exist.
--   @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError' \/ 'PermissionDenied'
--   The process has insufficient privileges to perform the operation.
--   @[EACCES]@
--
-- * 'ResourceExhausted'
--   Insufficient resources are available to perform the operation.
--   @[EMFILE, ENFILE]@
--
-- * 'InappropriateType'
--   The path refers to an existing non-directory object.
--   @[ENOTDIR]@

listDir :: MonadIO m
  => Path b Dir        -- ^ Directory to list
  -> m ([Path Abs Dir], [Path Abs File]) -- ^ Sub-directories and files
listDir path = do
  bpath <- makeAbsolute path
  (subdirs, files) <- listDirRel bpath
  return ( (bpath </>) <$> subdirs
         , (bpath </>) <$> files
         )

-- | The same as 'listDir' but returns relative paths.
--
-- @since 1.4.0

listDirRel :: MonadIO m
  => Path b Dir        -- ^ Directory to list
  -> m ([Path Rel Dir], [Path Rel File]) -- ^ Sub-directories and files
listDirRel path = liftIO $ do
  raw   <- liftD D.getDirectoryContents path
  items <- forM (raw \\ [".", ".."]) $ \item -> do
    isDir <- liftIO (D.doesDirectoryExist $ toFilePath path F.</> item)
    if isDir
      then Left  <$> parseRelDir  item
      else Right <$> parseRelFile item
  return (lefts items, rights items)

-- | Similar to 'listDir', but recursively traverses every sub-directory
-- /excluding symbolic links/, and returns all files and directories found.
-- This can fail with the same exceptions as 'listDir'.
--
-- __Note__: before version /1.3.0/, this function followed symlinks.

listDirRecur :: MonadIO m
  => Path b Dir                          -- ^ Directory to list
  -> m ([Path Abs Dir], [Path Abs File]) -- ^ Sub-directories and files
listDirRecur = listDirRecurWith walkDirAccum

-- | The same as 'listDirRecur' but returns relative paths.
--
-- @since 1.4.0

listDirRecurRel :: MonadIO m
  => Path b Dir                          -- ^ Directory to list
  -> m ([Path Rel Dir], [Path Rel File]) -- ^ Sub-directories and files
listDirRecurRel = listDirRecurWith walkDirAccumRel

-- | A non-public helper function used to define 'listDirRecur' and
-- 'listDirRecurRel'.

listDirRecurWith :: MonadIO m
  => (  Maybe (Path b Dir -> [Path b Dir] -> [Path b File] -> m (WalkAction b))
     -> (  Path b Dir
        -> [Path b Dir]
        -> [Path b File]
        -> m (DList.DList (Path b Dir), DList.DList (Path b File)))
     -> Path b' Dir
     -> m (DList.DList (Path b Dir), DList.DList (Path b File)))
     -- ^ The walk function to use
  -> Path b' Dir                     -- ^ Directory to list
  -> m ([Path b Dir], [Path b File]) -- ^ Sub-directories and files
listDirRecurWith walkF dir = (DList.toList *** DList.toList)
  <$> walkF (Just excludeSymlinks) writer dir
  where
    excludeSymlinks _ subdirs _ =
      WalkExclude <$> filterM isSymlink subdirs
    writer _ ds fs = return (DList.fromList ds, DList.fromList fs)

-- | Copies a directory recursively. It /does not/ follow symbolic links and
-- preserves permissions when possible. If the destination directory already
-- exists, new files and sub-directories complement its structure, possibly
-- overwriting old files if they happen to have the same name as the new
-- ones.
--
-- __Note__: before version /1.3.0/, this function followed symlinks.

copyDirRecur :: (MonadIO m, MonadCatch m)
  => Path b0 Dir       -- ^ Source
  -> Path b1 Dir       -- ^ Destination
  -> m ()
copyDirRecur = copyDirRecurGen True

-- | The same as 'copyDirRecur', but it /does not/ preserve directory
-- permissions. This may be useful, for example, if the directory you want
-- to copy is “read-only”, but you want your copy to be editable.
--
-- @since 1.1.0
--
-- __Note__: before version /1.3.0/, this function followed symlinks.

copyDirRecur' :: (MonadIO m, MonadCatch m)
  => Path b0 Dir       -- ^ Source
  -> Path b1 Dir       -- ^ Destination
  -> m ()
copyDirRecur' = copyDirRecurGen False

-- | Generic version of 'copyDirRecur'. The first argument controls whether
-- to preserve directory permissions or not. /Does not/ follow symbolic
-- links.
--
-- __Note__: before version /1.3.0/, this function followed symlinks.

copyDirRecurGen :: MonadIO m
  => Bool              -- ^ Should we preserve directory permissions?
  -> Path b0 Dir       -- ^ Source
  -> Path b1 Dir       -- ^ Destination
  -> m ()
copyDirRecurGen p src dest = liftIO $ do
  bsrc  <- makeAbsolute src
  bdest <- makeAbsolute dest
  (dirs, files) <- listDirRecur bsrc
  let swapParent
        :: Path Abs Dir
        -> Path Abs Dir
        -> Path Abs t
        -> IO (Path Abs t)
      swapParent old new path = (new </>) <$>
        stripProperPrefix old path
  tdirs  <- mapM (swapParent bsrc bdest) dirs
  tfiles <- mapM (swapParent bsrc bdest) files
  ensureDir bdest
  mapM_ ensureDir tdirs
  zipWithM_ copyFile files tfiles
  when p $ do
    ignoringIOErrors (copyPermissions bsrc bdest)
    zipWithM_ (\s d -> ignoringIOErrors $ copyPermissions s d) dirs tdirs

----------------------------------------------------------------------------
-- Walking directory trees

-- Recursive directory walk functionality, with a flexible API and avoidance
-- of loops. Following are some notes on the design.
--
-- Callback handler API:
--
-- The callback handler interface is designed to be highly flexible. There are
-- two possible alternative ways to control the traversal:
--
-- * In the context of the parent dir, decide which subdirs to descend into.
-- * In the context of the subdir, decide whether to traverse the subdir or not.
--
-- We choose the first approach here since it is more flexible and can
-- achieve everything that the second one can. The additional benefit with
-- this is that we can use the parent dir context efficiently instead of
-- each child looking at the parent context independently.
--
-- To control which subdirs to descend we use a 'WalkExclude' API instead of
-- a “WalkInclude” type of API so that the handlers cannot accidentally ask
-- us to descend a dir which is not a subdir of the directory being walked.
--
-- Avoiding Traversal Loops:
--
-- There can be loops in the path being traversed due to subdirectory
-- symlinks or filesystem corruptions can cause loops by creating directory
-- hardlinks. Also, if the filesystem is changing while we are traversing
-- then we might be going in loops due to the changes.
--
-- We record the path we are coming from to detect the loops. If we end up
-- traversing the same directory again we are in a loop.

-- | Action returned by the traversal handler function. The action controls
-- how the traversal will proceed.
--
-- __Note__: in version /1.4.0/ the type was adjusted to have the @b@ type
-- parameter.
--
-- @since 1.2.0

data WalkAction b
  = WalkFinish                  -- ^ Finish the entire walk altogether
  | WalkExclude [Path b Dir]    -- ^ List of sub-directories to exclude from
                                -- descending
  deriving (Eq, Show)

-- | Traverse a directory tree using depth first pre-order traversal,
-- calling a handler function at each directory node traversed. The absolute
-- paths of the parent directory, sub-directories and the files in the
-- directory are provided as arguments to the handler.
--
-- The function is capable of detecting and avoiding traversal loops in the
-- directory tree. Note that the traversal follows symlinks by default, an
-- appropriate traversal handler can be used to avoid that when necessary.
--
-- @since 1.2.0

walkDir
  :: MonadIO m
  => (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (WalkAction Abs))
     -- ^ Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  -> Path b Dir
     -- ^ Directory where traversal begins
  -> m ()
walkDir handler topdir = void $
  makeAbsolute topdir >>= walkAvoidLoop S.empty
  where
    walkAvoidLoop traversed curdir = do
      mRes <- checkLoop traversed curdir
      case mRes of
        Nothing -> return $ Just ()
        Just traversed' -> walktree traversed' curdir

    -- use Maybe monad to abort any further traversal if any of the
    -- handler calls returns WalkFinish
    walktree traversed curdir = do
      (subdirs, files) <- listDir curdir
      action <- handler curdir subdirs files
      case action of
        WalkFinish -> return Nothing
        WalkExclude xdirs ->
          case subdirs \\ xdirs of
            [] -> return $ Just ()
            ds -> runMaybeT $ mapM_ (MaybeT . walkAvoidLoop traversed) ds

    checkLoop traversed dir = do
      st <- liftIO $ P.getFileStatus (toFilePath dir)
      let ufid = (P.deviceID st, P.fileID st)

      -- check for loop, have we already traversed this dir?
      return $ if S.member ufid traversed
        then Nothing
        else Just (S.insert ufid traversed)

-- | The same as 'walkDir' but uses relative paths.
--
-- @since 1.4.0

walkDirRel
  :: MonadIO m
  => (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (WalkAction Rel))
     -- ^ Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  -> Path b Dir
     -- ^ Directory where traversal begins
  -> m ()
walkDirRel handler' topdir' = do
  topdir <- makeAbsolute topdir'
  let stripTopdir :: MonadIO m => Path Abs f -> m (Path Rel f)
      stripTopdir = liftIO .
        stripProperPrefix topdir
      handler curdir subdirs files = do
        curdirRel  <- if curdir == topdir
          then return $(mkRelDir ".")
          else stripTopdir curdir
        subdirsRel <- mapM stripTopdir subdirs
        filesRel   <- mapM stripTopdir files
        action     <- handler' curdirRel subdirsRel filesRel
        return $ case action of
          WalkFinish ->  WalkFinish
          WalkExclude xdirs -> WalkExclude $
            (topdir </>) <$> xdirs
  walkDir handler topdir

-- | Similar to 'walkDir' but accepts a 'Monoid'-returning output writer as
-- well. Values returned by the output writer invocations are accumulated
-- and returned.
--
-- Both, the descend handler as well as the output writer can be used for
-- side effects but keep in mind that the output writer runs before the
-- descend handler.
--
-- @since 1.2.0

walkDirAccum
  :: (MonadIO m, Monoid o)
  => Maybe
       (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (WalkAction Abs))
    -- ^ Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
    -- descend the whole tree if omitted
  -> (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m o)
     -- ^ Output writer (@dir -> subdirs -> files -> o@)
  -> Path b Dir
     -- ^ Directory where traversal begins
  -> m o
     -- ^ Accumulation of outputs generated by the output writer invocations
walkDirAccum = walkDirAccumWith walkDir

-- | The same as 'walkDirAccum' but uses relative paths.
--
-- @since 1.4.0

walkDirAccumRel
  :: (MonadIO m, Monoid o)
  => Maybe
       (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (WalkAction Rel))
    -- ^ Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
    -- descend the whole tree if omitted
  -> (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m o)
     -- ^ Output writer (@dir -> subdirs -> files -> o@)
  -> Path b Dir
     -- ^ Directory where traversal begins
  -> m o
     -- ^ Accumulation of outputs generated by the output writer invocations
walkDirAccumRel = walkDirAccumWith walkDirRel

walkDirAccumWith
  :: (MonadIO m, Monoid o)
  => (  (  Path a Dir
        -> [Path a Dir]
        -> [Path a File]
        -> WriterT o m (WalkAction a)
        )
        -> Path b Dir
        -> WriterT o m ()
     )
    -- ^ The walk function we use
  -> Maybe (Path a Dir -> [Path a Dir] -> [Path a File] -> m (WalkAction a))
    -- ^ Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
    -- descend the whole tree if omitted
  -> (Path a Dir -> [Path a Dir] -> [Path a File] -> m o)
     -- ^ Output writer (@dir -> subdirs -> files -> o@)
  -> Path b Dir
     -- ^ Directory where traversal begins
  -> m o
     -- ^ Accumulation of outputs generated by the output writer invocations
walkDirAccumWith walkF dHandler writer topdir =
  execWriterT (walkF handler topdir)
  where
    handler dir subdirs files = do
      res <- lift $ writer dir subdirs files
      tell res
      case dHandler of
        Just h -> lift $ h dir subdirs files
        Nothing -> return (WalkExclude [])

----------------------------------------------------------------------------
-- Current working directory

-- | Obtain the current working directory as an absolute path.
--
-- In a multithreaded program, the current working directory is a global
-- state shared among all threads of the process. Therefore, when performing
-- filesystem operations from multiple threads, it is highly recommended to
-- use absolute rather than relative paths (see: 'makeAbsolute').
--
-- The operation may fail with:
--
-- * 'HardwareFault'
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * 'isDoesNotExistError' or 'NoSuchThing'
-- There is no path referring to the working directory.
-- @[EPERM, ENOENT, ESTALE...]@
--
-- * 'isPermissionError' or 'PermissionDenied'
-- The process has insufficient privileges to perform the operation.
-- @[EACCES]@
--
-- * 'ResourceExhausted'
-- Insufficient resources are available to perform the operation.
--
-- * 'UnsupportedOperation'
-- The operating system has no notion of current working directory.

getCurrentDir :: MonadIO m => m (Path Abs Dir)
getCurrentDir = liftIO $ D.getCurrentDirectory >>= parseAbsDir

-- | Change the working directory to the given path.
--
-- In a multithreaded program, the current working directory is a global
-- state shared among all threads of the process. Therefore, when performing
-- filesystem operations from multiple threads, it is highly recommended to
-- use absolute rather than relative paths (see: 'makeAbsolute').
--
-- The operation may fail with:
--
-- * 'HardwareFault'
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * 'InvalidArgument'
-- The operand is not a valid directory name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError' or 'NoSuchThing'
-- The directory does not exist.
-- @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError' or 'PermissionDenied'
-- The process has insufficient privileges to perform the operation.
-- @[EACCES]@
--
-- * 'UnsupportedOperation'
-- The operating system has no notion of current working directory, or the
-- working directory cannot be dynamically changed.
--
-- * 'InappropriateType'
-- The path refers to an existing non-directory object.
-- @[ENOTDIR]@

setCurrentDir :: MonadIO m => Path b Dir -> m ()
setCurrentDir = liftD D.setCurrentDirectory

-- | Run an 'IO' action with the given working directory and restore the
-- original working directory afterwards, even if the given action fails due
-- to an exception.
--
-- The operation may fail with the same exceptions as 'getCurrentDir' and
-- 'setCurrentDir'.

withCurrentDir :: (MonadIO m, MonadMask m)
  => Path b Dir        -- ^ Directory to execute in
  -> m a               -- ^ Action to be executed
  -> m a
withCurrentDir dir action =
  bracket getCurrentDir setCurrentDir $ const (setCurrentDir dir >> action)

----------------------------------------------------------------------------
-- Pre-defined directories

-- | Return the current user's home directory.
--
-- The directory returned is expected to be writable by the current user,
-- but note that it isn't generally considered good practice to store
-- application-specific data here; use 'getAppUserDataDir' instead.
--
-- On Unix, 'getHomeDir' returns the value of the @HOME@ environment
-- variable. On Windows, the system is queried for a suitable path; a
-- typical path might be @C:\/Users\//\<user\>/@.
--
-- The operation may fail with:
--
-- * 'UnsupportedOperation'
-- The operating system has no notion of home directory.
--
-- * 'isDoesNotExistError'
-- The home directory for the current user does not exist, or
-- cannot be found.

getHomeDir :: MonadIO m => m (Path Abs Dir)
getHomeDir = liftIO D.getHomeDirectory >>= resolveDir'

-- | Obtain the path to a special directory for storing user-specific
-- application data (traditional Unix location).
--
-- The argument is usually the name of the application. Since it will be
-- integrated into the path, it must consist of valid path characters.
--
-- * On Unix-like systems, the path is @~\/./\<app\>/@.
-- * On Windows, the path is @%APPDATA%\//\<app\>/@
--   (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming\//\<app\>/@)
--
-- Note: the directory may not actually exist, in which case you would need
-- to create it. It is expected that the parent directory exists and is
-- writable.
--
-- The operation may fail with:
--
-- * 'UnsupportedOperation'
--   The operating system has no notion of application-specific data
--   directory.
--
-- * 'isDoesNotExistError'
--   The home directory for the current user does not exist, or cannot be
--   found.

getAppUserDataDir :: MonadIO m
  => String            -- ^ Name of application (used in path construction)
  -> m (Path Abs Dir)
getAppUserDataDir = liftIO . (>>= parseAbsDir) . D.getAppUserDataDirectory

-- | Return the current user's document directory.
--
-- The directory returned is expected to be writable by the current user,
-- but note that it isn't generally considered good practice to store
-- application-specific data here; use 'getAppUserDataDir' instead.
--
-- On Unix, 'getUserDocsDir' returns the value of the @HOME@ environment
-- variable. On Windows, the system is queried for a suitable path; a
-- typical path might be @C:\/Users\//\<user\>/\/Documents@.
--
-- The operation may fail with:
--
-- * 'UnsupportedOperation'
-- The operating system has no notion of document directory.
--
-- * 'isDoesNotExistError'
-- The document directory for the current user does not exist, or
-- cannot be found.

getUserDocsDir :: MonadIO m => m (Path Abs Dir)
getUserDocsDir = liftIO $ D.getUserDocumentsDirectory >>= parseAbsDir

-- | Return the current directory for temporary files.
--
-- On Unix, 'getTempDir' returns the value of the @TMPDIR@ environment
-- variable or \"\/tmp\" if the variable isn\'t defined. On Windows, the
-- function checks for the existence of environment variables in the
-- following order and uses the first path found:
--
-- *
-- TMP environment variable.
--
-- *
-- TEMP environment variable.
--
-- *
-- USERPROFILE environment variable.
--
-- *
-- The Windows directory
--
-- The operation may fail with:
--
-- * 'UnsupportedOperation'
-- The operating system has no notion of temporary directory.
--
-- The function doesn't verify whether the path exists.

getTempDir :: MonadIO m => m (Path Abs Dir)
getTempDir = liftIO D.getTemporaryDirectory >>= resolveDir'

#if MIN_VERSION_directory(1,2,3)
-- | Obtain the paths to special directories for storing user-specific
-- application data, configuration, and cache files, conforming to the
-- <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
-- Compared with 'getAppUserDataDir', this function provides a more
-- fine-grained hierarchy as well as greater flexibility for the user.
--
-- It also works on Windows, although in that case 'XdgData' and 'XdgConfig'
-- will map to the same directory.
--
-- Note: The directory may not actually exist, in which case you would need
-- to create it with file mode @700@ (i.e. only accessible by the owner).
--
-- Note also: this is a piece of conditional API, only available if
-- @directory-1.2.3.0@ or later is used.
--
-- @since 1.2.1

getXdgDir :: MonadIO m
  => XdgDirectory      -- ^ Which special directory
  -> Maybe (Path Rel Dir)
     -- ^ A relative path that is appended to the path; if 'Nothing', the
     -- base path is returned
  -> m (Path Abs Dir)
getXdgDir xdgDir suffix =
  liftIO $ (D.getXdgDirectory xdgDir $ maybe "" toFilePath suffix) >>= parseAbsDir
#endif

----------------------------------------------------------------------------
-- Path transformation

-- | Class of things ('Path's) that can be canonicalized, made absolute, and
-- made relative to a some base directory.

class AnyPath path where

  -- | Type of absolute version of the given @path@.

  type AbsPath path :: *

  -- | Type of relative version of the given @path@.

  type RelPath path :: *

  -- | Make a path absolute and remove as many indirections from it as
  -- possible. Indirections include the two special directories @.@ and
  -- @..@, as well as any symbolic links. The input path need not point to
  -- an existing file or directory.
  --
  -- __Note__: if you require only an absolute path, use 'makeAbsolute'
  -- instead. Most programs need not care about whether a path contains
  -- symbolic links.
  --
  -- Due to the fact that symbolic links are dependent on the state of the
  -- existing filesystem, the function can only make a conservative,
  -- best-effort attempt. Nevertheless, if the input path points to an
  -- existing file or directory, then the output path shall also point to
  -- the same file or directory.
  --
  -- Formally, symbolic links are removed from the longest prefix of the
  -- path that still points to an existing file. The function is not atomic,
  -- therefore concurrent changes in the filesystem may lead to incorrect
  -- results.
  --
  -- (Despite the name, the function does not guarantee canonicity of the
  -- returned path due to the presence of hard links, mount points, etc.)
  --
  -- /Known bug(s)/: on Windows, the function does not resolve symbolic
  -- links.
  --
  -- Please note that before version 1.2.3.0 of the @directory@ package,
  -- this function had unpredictable behavior on non-existent paths.

  canonicalizePath :: MonadIO m
    => path
    -> m (AbsPath path)

  -- | Make a path absolute by prepending the current directory (if it isn't
  -- already absolute) and applying 'F.normalise' to the result.
  --
  -- If the path is already absolute, the operation never fails. Otherwise,
  -- the operation may fail with the same exceptions as 'getCurrentDir'.

  makeAbsolute :: MonadIO m
    => path
    -> m (AbsPath path)

  -- | Make a path relative to a given directory.
  --
  -- @since 0.3.0

  makeRelative :: MonadThrow m
    => Path Abs Dir    -- ^ Base directory
    -> path            -- ^ Path that will be made relative to base directory
    -> m (RelPath path)

  -- | Make a path relative to current working directory.
  --
  -- @since 0.3.0

  makeRelativeToCurrentDir :: MonadIO m
    => path
    -> m (RelPath path)

instance AnyPath (Path b File) where

  type AbsPath (Path b File) = Path Abs File
  type RelPath (Path b File) = Path Rel File

  canonicalizePath = liftD $ D.canonicalizePath >=> parseAbsFile
  makeAbsolute     = liftD $ D.makeAbsolute     >=> parseAbsFile
  makeRelative b p = parseRelFile (F.makeRelative (toFilePath b) (toFilePath p))
  makeRelativeToCurrentDir p = liftIO $ getCurrentDir >>= flip makeRelative p

instance AnyPath (Path b Dir) where

  type AbsPath (Path b Dir) = Path Abs Dir
  type RelPath (Path b Dir) = Path Rel Dir

  canonicalizePath = liftD D.canonicalizePath >=> liftIO . parseAbsDir
  makeAbsolute     = liftD D.makeAbsolute     >=> liftIO . parseAbsDir
  makeRelative b p = parseRelDir (F.makeRelative (toFilePath b) (toFilePath p))
  makeRelativeToCurrentDir p = liftIO $ getCurrentDir >>= flip makeRelative p

-- | Append stringly-typed path to an absolute path and then canonicalize
-- it.
--
-- @since 0.3.0

resolveFile :: MonadIO m
  => Path Abs Dir      -- ^ Base directory
  -> FilePath          -- ^ Path to resolve
  -> m (Path Abs File)
resolveFile b p = liftIO $ D.canonicalizePath (toFilePath b F.</> p) >>= parseAbsFile

-- | The same as 'resolveFile', but uses current working directory.
--
-- @since 0.3.0

resolveFile' :: MonadIO m
  => FilePath          -- ^ Path to resolve
  -> m (Path Abs File)
resolveFile' p = getCurrentDir >>= flip resolveFile p

-- | The same as 'resolveFile', but for directories.
--
-- @since 0.3.0

resolveDir :: MonadIO m
  => Path Abs Dir      -- ^ Base directory
  -> FilePath          -- ^ Path to resolve
  -> m (Path Abs Dir)
resolveDir b p = liftIO $ D.canonicalizePath (toFilePath b F.</> p) >>= parseAbsDir

-- | The same as 'resolveDir', but uses current working directory.
--
-- @since 0.3.0

resolveDir' :: MonadIO m
  => FilePath          -- ^ Path to resolve
  -> m (Path Abs Dir)
resolveDir' p = getCurrentDir >>= flip resolveDir p

----------------------------------------------------------------------------
-- Actions on files

-- | @'removeFile' file@ removes the directory entry for an existing file
-- @file@, where @file@ is not itself a directory. The implementation may
-- specify additional constraints which must be satisfied before a file can
-- be removed (e.g. the file may not be in use by other processes).
--
-- The operation may fail with:
--
-- * 'HardwareFault'
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * 'InvalidArgument'
-- The operand is not a valid file name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError' \/ 'NoSuchThing'
-- The file does not exist.
-- @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError' \/ 'PermissionDenied'
-- The process has insufficient privileges to perform the operation.
-- @[EROFS, EACCES, EPERM]@
--
-- * 'UnsatisfiedConstraints'
-- Implementation-dependent constraints are not satisfied.
-- @[EBUSY]@
--
-- * 'InappropriateType'
-- The operand refers to an existing directory.
-- @[EPERM, EINVAL]@

removeFile :: MonadIO m => Path b File -> m ()
removeFile = liftD D.removeFile

-- | @'renameFile' old new@ changes the name of an existing file system
-- object from /old/ to /new/. If the /new/ object already exists, it is
-- atomically replaced by the /old/ object. Neither path may refer to an
-- existing directory. A conformant implementation need not support renaming
-- files in all situations (e.g. renaming across different physical
-- devices), but the constraints must be documented.
--
-- The operation may fail with:
--
-- * 'HardwareFault'
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * 'InvalidArgument'
-- Either operand is not a valid file name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError' \/ 'NoSuchThing'
-- The original file does not exist, or there is no path to the target.
-- @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError' \/ 'PermissionDenied'
-- The process has insufficient privileges to perform the operation.
-- @[EROFS, EACCES, EPERM]@
--
-- * 'ResourceExhausted'
-- Insufficient resources are available to perform the operation.
-- @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
--
-- * 'UnsatisfiedConstraints'
-- Implementation-dependent constraints are not satisfied.
-- @[EBUSY]@
--
-- * 'UnsupportedOperation'
-- The implementation does not support renaming in this situation.
-- @[EXDEV]@
--
-- * 'InappropriateType'
-- Either path refers to an existing directory.
-- @[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@

renameFile :: MonadIO m
  => Path b0 File      -- ^ Original location
  -> Path b1 File      -- ^ New location
  -> m ()
renameFile = liftD2 D.renameFile

-- | @'copyFile' old new@ copies the existing file from @old@ to @new@. If
-- the @new@ file already exists, it is atomically replaced by the @old@
-- file. Neither path may refer to an existing directory. The permissions of
-- @old@ are copied to @new@, if possible.

copyFile :: MonadIO m
  => Path b0 File      -- ^ Original location
  -> Path b1 File      -- ^ Where to put copy
  -> m ()
copyFile = liftD2 D.copyFile

-- | Given an executable file name, search for such file in the directories
-- listed in system @PATH@. The returned value is the path to the found
-- executable or 'Nothing' if an executable with the given name was not
-- found. For example ('findExecutable' \"ghc\") gives you the path to GHC.
--
-- The path returned by 'findExecutable' corresponds to the program that
-- would be executed by 'System.Process.createProcess' when passed the same
-- string (as a RawCommand, not a ShellCommand).
--
-- On Windows, 'findExecutable' calls the Win32 function 'SearchPath', which
-- may search other places before checking the directories in @PATH@. Where
-- it actually searches depends on registry settings, but notably includes
-- the directory containing the current executable. See
-- <http://msdn.microsoft.com/en-us/library/aa365527.aspx> for more details.

findExecutable :: MonadIO m
  => Path Rel File     -- ^ Executable file name
  -> m (Maybe (Path Abs File)) -- ^ Path to found executable
findExecutable = fmap (>>= parseAbsFile) . liftD D.findExecutable

-- | Search through the given set of directories for the given file.

findFile :: MonadIO m
  => [Path b Dir]      -- ^ Set of directories to search in
  -> Path Rel File     -- ^ Filename of interest
  -> m (Maybe (Path Abs File)) -- ^ Absolute path to file (if found)
findFile [] _ = return Nothing
findFile (d:ds) file = do
  bfile <- (</> file) <$> makeAbsolute d
  exist <- doesFileExist bfile
  if exist
    then return (Just bfile)
    else findFile ds file

-- | Search through the given set of directories for the given file and
-- return a list of paths where the given file exists.

findFiles :: MonadIO m
  => [Path b Dir]      -- ^ Set of directories to search in
  -> Path Rel File     -- ^ Filename of interest
  -> m [Path Abs File] -- ^ Absolute paths to all found files
findFiles = findFilesWith (const (return True))

-- | Search through the given set of directories for the given file and with
-- the given property (usually permissions) and return a list of paths where
-- the given file exists and has the property.

findFilesWith :: MonadIO m
  => (Path Abs File -> m Bool) -- ^ How to test the files
  -> [Path b Dir]      -- ^ Set of directories to search in
  -> Path Rel File     -- ^ Filename of interest
  -> m [Path Abs File] -- ^ Absolute paths to all found files
findFilesWith _ [] _ = return []
findFilesWith f (d:ds) file = do
  bfile <- (</> file) <$> makeAbsolute d
  exist <- doesFileExist file
  b <- if exist then f bfile else return False
  if b
    then (bfile:) <$> findFilesWith f ds file
    else findFilesWith f ds file

----------------------------------------------------------------------------
-- Symbolic links

-- | Check if the given path is a symbolic link.
--
-- @since 1.3.0

isSymlink :: MonadIO m => Path b t -> m Bool
isSymlink p = liftIO (P.isSymbolicLink <$> P.getSymbolicLinkStatus path)
  where
    -- NOTE: To be able to correctly check whether it is a symlink or not we
    -- have to drop the trailing separator from the dir path.
    path = F.dropTrailingPathSeparator (toFilePath p)

----------------------------------------------------------------------------
-- Temporary files and directories

-- | Use a temporary file that doesn't already exist.
--
-- Creates a new temporary file inside the given directory, making use of
-- the template. The temporary file is deleted after use.
--
-- @since 0.2.0

withTempFile :: (MonadIO m, MonadMask m)
  => Path b Dir        -- ^ Directory to create the file in
  -> String            -- ^ File name template, see 'openTempFile'
  -> (Path Abs File -> Handle -> m a) -- ^ Callback that can use the file
  -> m a
withTempFile path t action = do
  apath <- makeAbsolute path
  T.withTempFile (toFilePath apath) t $ \file h ->
    parseAbsFile file >>= flip action h

-- | Create and use a temporary directory.
--
-- Creates a new temporary directory inside the given directory, making use
-- of the template. The temporary directory is deleted after use.
--
-- @since 0.2.0

withTempDir :: (MonadIO m, MonadMask m)
  => Path b Dir        -- ^ Directory to create the file in
  -> String            -- ^ Directory name template, see 'openTempFile'
  -> (Path Abs Dir -> m a) -- ^ Callback that can use the directory
  -> m a
withTempDir path t action = do
  apath <- makeAbsolute path
  T.withTempDirectory (toFilePath apath) t (parseAbsDir >=> action)

-- | Create and use a temporary file in the system standard temporary
-- directory.
--
-- Behaves exactly the same as 'withTempFile', except that the parent
-- temporary directory will be that returned by 'getTempDir'.
--
-- @since 0.2.0

withSystemTempFile :: (MonadIO m, MonadMask m)
  => String            -- ^ File name template, see 'openTempFile'
  -> (Path Abs File -> Handle -> m a) -- ^ Callback that can use the file
  -> m a
withSystemTempFile t action = getTempDir >>= \path ->
  withTempFile path t action

-- | Create and use a temporary directory in the system standard temporary
-- directory.
--
-- Behaves exactly the same as 'withTempDir', except that the parent
-- temporary directory will be that returned by 'getTempDir'.
--
-- @since 0.2.0

withSystemTempDir :: (MonadIO m, MonadMask m)
  => String            -- ^ Directory name template, see 'openTempFile'
  -> (Path Abs Dir -> m a) -- ^ Callback that can use the directory
  -> m a
withSystemTempDir t action = getTempDir >>= \path ->
  withTempDir path t action

-- | The function creates a temporary file in @rw@ mode. The created file
-- isn't deleted automatically, so you need to delete it manually.
--
-- The file is created with permissions such that only the current user can
-- read\/write it.
--
-- With some exceptions (see below), the file will be created securely in
-- the sense that an attacker should not be able to cause openTempFile to
-- overwrite another file on the filesystem using your credentials, by
-- putting symbolic links (on Unix) in the place where the temporary file is
-- to be created. On Unix the @O_CREAT@ and @O_EXCL@ flags are used to
-- prevent this attack, but note that @O_EXCL@ is sometimes not supported on
-- NFS filesystems, so if you rely on this behaviour it is best to use local
-- filesystems only.
--
-- @since 0.2.0

openTempFile :: MonadIO m
  => Path b Dir        -- ^ Directory to create file in
  -> String
     -- ^ File name template; if the template is "foo.ext" then the created
     -- file will be @\"fooXXX.ext\"@ where @XXX@ is some random number
  -> m (Path Abs File, Handle) -- ^ Name of created file and its 'Handle'
openTempFile path t = liftIO $ do
  apath <- makeAbsolute path
  (tfile, h) <- liftD2' T.openTempFile apath t
  (,h) <$> parseAbsFile tfile

-- | Like 'openTempFile', but opens the file in binary mode. On Windows,
-- reading a file in text mode (which is the default) will translate @CRLF@
-- to @LF@, and writing will translate @LF@ to @CRLF@. This is usually what
-- you want with text files. With binary files this is undesirable; also, as
-- usual under Microsoft operating systems, text mode treats control-Z as
-- EOF. Binary mode turns off all special treatment of end-of-line and
-- end-of-file characters.
--
-- @since 0.2.0

openBinaryTempFile :: MonadIO m
  => Path b Dir        -- ^ Directory to create file in
  -> String            -- ^ File name template, see 'openTempFile'
  -> m (Path Abs File, Handle) -- ^ Name of created file and its 'Handle'
openBinaryTempFile path t = liftIO $ do
  apath <- makeAbsolute path
  (tfile, h) <- liftD2' T.openBinaryTempFile apath t
  (,h) <$> parseAbsFile tfile

-- | Create a temporary directory. The created directory isn't deleted
-- automatically, so you need to delete it manually.
--
-- The directory is created with permissions such that only the current user
-- can read\/write it.
--
-- @since 0.2.0

createTempDir :: MonadIO m
  => Path b Dir        -- ^ Directory to create file in
  -> String            -- ^ Directory name template, see 'openTempFile'
  -> m (Path Abs Dir)  -- ^ Name of created temporary directory
createTempDir path t = liftIO $ makeAbsolute path >>= \apath ->
  liftD2' T.createTempDirectory apath t >>= parseAbsDir

----------------------------------------------------------------------------
-- Existence tests

-- | The operation 'doesFileExist' returns 'True' if the argument file
-- exists and is not a directory, and 'False' otherwise.

doesFileExist :: MonadIO m => Path b File -> m Bool
doesFileExist = liftD D.doesFileExist

-- | The operation 'doesDirExist' returns 'True' if the argument file exists
-- and is either a directory or a symbolic link to a directory, and 'False'
-- otherwise.

doesDirExist :: MonadIO m => Path b Dir -> m Bool
doesDirExist = liftD D.doesDirectoryExist

-- | Check if there is a file or directory on specified path.

isLocationOccupied :: MonadIO m => Path b t -> m Bool
isLocationOccupied path = do
  let fp = toFilePath path
  file <- liftIO (D.doesFileExist fp)
  dir  <- liftIO (D.doesDirectoryExist fp)
  return (file || dir)

-- | If argument of the function throws a
-- 'System.IO.Error.doesNotExistErrorType', 'Nothing' is returned (other
-- exceptions propagate). Otherwise the result is returned inside a 'Just'.
--
-- @since 0.3.0

forgivingAbsence :: (MonadIO m, MonadCatch m) => m a -> m (Maybe a)
forgivingAbsence f = catchIf isDoesNotExistError
  (Just <$> f)
  (const $ return Nothing)

-- | The same as 'forgivingAbsence', but ignores result.
--
-- @since 0.3.1

ignoringAbsence :: (MonadIO m, MonadCatch m) => m a -> m ()
ignoringAbsence = void . forgivingAbsence

----------------------------------------------------------------------------
-- Permissions

-- | The 'getPermissions' operation returns the permissions for the file or
-- directory.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to access
--   the permissions; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.

getPermissions :: MonadIO m => Path b t -> m D.Permissions
getPermissions = liftD D.getPermissions

-- | The 'setPermissions' operation sets the permissions for the file or
-- directory.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to set
--   the permissions; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.

setPermissions :: MonadIO m => Path b t -> D.Permissions -> m ()
setPermissions = liftD2' D.setPermissions

-- | Set permissions for the object found on second given path so they match
-- permissions of the object on the first path.

copyPermissions :: MonadIO m
  => Path b0 t0        -- ^ From where to copy
  -> Path b1 t1        -- ^ What to modify
  -> m ()
copyPermissions = liftD2 D.copyPermissions

----------------------------------------------------------------------------
-- Timestamps

#if MIN_VERSION_directory(1,2,3)

-- | Obtain the time at which the file or directory was last accessed.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to read
--   the access time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- Caveat for POSIX systems: This function returns a timestamp with
-- sub-second resolution only if this package is compiled against
-- @unix-2.6.0.0@ or later and the underlying filesystem supports them.
--
-- Note: this is a piece of conditional API, only available if
-- @directory-1.2.3.0@ or later is used.

getAccessTime :: MonadIO m => Path b t -> m UTCTime
getAccessTime = liftD D.getAccessTime

-- | Change the time at which the file or directory was last accessed.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to alter the
--   access time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- Some caveats for POSIX systems:
--
-- * Not all systems support @utimensat@, in which case the function can
--   only emulate the behavior by reading the modification time and then
--   setting both the access and modification times together. On systems
--   where @utimensat@ is supported, the access time is set atomically with
--   nanosecond precision.
--
-- * If compiled against a version of @unix@ prior to @2.7.0.0@, the
--   function would not be able to set timestamps with sub-second
--   resolution. In this case, there would also be loss of precision in the
--   modification time.
--
-- Note: this is a piece of conditional API, only available if
-- @directory-1.2.3.0@ or later is used.

setAccessTime :: MonadIO m => Path b t -> UTCTime -> m ()
setAccessTime = liftD2' D.setAccessTime

-- | Change the time at which the file or directory was last modified.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to alter the
--   modification time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- Some caveats for POSIX systems:
--
-- * Not all systems support @utimensat@, in which case the function can
--   only emulate the behavior by reading the access time and then setting
--   both the access and modification times together. On systems where
--   @utimensat@ is supported, the modification time is set atomically with
--   nanosecond precision.
--
-- * If compiled against a version of @unix@ prior to @2.7.0.0@, the
--   function would not be able to set timestamps with sub-second
--   resolution. In this case, there would also be loss of precision in the
--   access time.
--
-- Note: this is a piece of conditional API, only available if
-- @directory-1.2.3.0@ or later is used.

setModificationTime :: MonadIO m => Path b t -> UTCTime -> m ()
setModificationTime = liftD2' D.setModificationTime
#endif

-- | Obtain the time at which the file or directory was last modified.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to read
--   the modification time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- Caveat for POSIX systems: This function returns a timestamp with
-- sub-second resolution only if this package is compiled against
-- @unix-2.6.0.0@ or later and the underlying filesystem supports them.

getModificationTime :: MonadIO m => Path b t -> m UTCTime
getModificationTime = liftD D.getModificationTime

----------------------------------------------------------------------------
-- Helpers

-- | Lift action in 'IO' that takes 'FilePath' into action in slightly more
-- abstract monad that takes 'Path'.

liftD :: MonadIO m
  => (FilePath -> IO a) -- ^ Original action
  -> Path b t          -- ^ 'Path' argument
  -> m a               -- ^ Lifted action
liftD m = liftIO . m . toFilePath
{-# INLINE liftD #-}

-- | Similar to 'liftD' for functions with arity 2.

liftD2 :: MonadIO m
  => (FilePath -> FilePath -> IO a) -- ^ Original action
  -> Path b0 t0        -- ^ First 'Path' argument
  -> Path b1 t1        -- ^ Second 'Path' argument
  -> m a
liftD2 m a b = liftIO $ m (toFilePath a) (toFilePath b)
{-# INLINE liftD2 #-}

-- | Similar to 'liftD2', but allows to pass second argument of arbitrary
-- type.

liftD2' :: MonadIO m
  => (FilePath -> v -> IO a) -- ^ Original action
  -> Path b t          -- ^ First 'Path' argument
  -> v                 -- ^ Second argument
  -> m a
liftD2' m a v = liftIO $ m (toFilePath a) v
{-# INLINE liftD2' #-}

-- | Perform an action ignoring IO exceptions it may throw.

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `catch` handler
  where
    handler :: Monad m => IOError -> m ()
    handler = const (return ())
