{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Path.IO
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides an interface to "System.Directory" for users of the
-- "Path" module. It also implements some extra functionality like recursive
-- scanning and copying of directories, working with temporary
-- files\/directories, etc.
module Path.IO
  ( -- * Actions on directories
    createDir,
    createDirIfMissing,
    ensureDir,
    removeDir,
    removeDirRecur,
    removePathForcibly,
    renameDir,
    renamePath,
    listDir,
    listDirRel,
    listDirRecur,
    listDirRecurRel,
    copyDirRecur,
    copyDirRecur',

    -- ** Walking directory trees
    WalkAction (..),
    walkDir,
    walkDirRel,
    walkDirAccum,
    walkDirAccumRel,

    -- ** Current working directory
    getCurrentDir,
    setCurrentDir,
    withCurrentDir,

    -- * Pre-defined directories
    getHomeDir,
    getAppUserDataDir,
    getUserDocsDir,
    getTempDir,
    D.XdgDirectory (..),
    getXdgDir,
    D.XdgDirectoryList (..),
    getXdgDirList,

    -- * Path transformation
    AnyPath (..),
    resolveFile,
    resolveFile',
    resolveDir,
    resolveDir',

    -- * Actions on files
    removeFile,
    renameFile,
    copyFile,
    getFileSize,
    findExecutable,
    findFile,
    findFiles,
    findFilesWith,

    -- * Symbolic links
    createFileLink,
    createDirLink,
    removeDirLink,
    getSymlinkTarget,
    isSymlink,

    -- * Temporary files and directories
    withTempFile,
    withTempDir,
    withSystemTempFile,
    withSystemTempDir,
    openTempFile,
    openBinaryTempFile,
    createTempDir,

    -- * Existence tests
    doesPathExist,
    doesFileExist,
    doesDirExist,
    isLocationOccupied,
    forgivingAbsence,
    ignoringAbsence,

    -- * Permissions
    D.Permissions,
    D.emptyPermissions,
    D.readable,
    D.writable,
    D.executable,
    D.searchable,
    D.setOwnerReadable,
    D.setOwnerWritable,
    D.setOwnerExecutable,
    D.setOwnerSearchable,
    getPermissions,
    setPermissions,
    copyPermissions,

    -- * Timestamps
    getAccessTime,
    setAccessTime,
    setModificationTime,
    getModificationTime,
  )
where

import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Writer.Strict (WriterT, execWriterT, tell)
import Data.DList qualified as DList
import Data.Either (lefts, rights)
import Data.Kind (Type)
import Data.List ((\\))
import Data.Set qualified as S
import Data.Time (UTCTime)
import Path
import System.Directory qualified as D
import System.FilePath qualified as F
import System.IO (Handle)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp qualified as T
import System.PosixCompat.Files qualified as P

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
createDir :: (MonadIO m) => Path b Dir -> m ()
createDir = liftD D.createDirectory

-- | @'createDirIfMissing' parents dir@ creates a new directory @dir@ if it
-- doesn't exist. If the first argument is 'True' the function will also
-- create all parent directories if they are missing.
createDirIfMissing ::
  (MonadIO m) =>
  -- | Create its parents too?
  Bool ->
  -- | The path to the directory you want to make
  Path b Dir ->
  m ()
createDirIfMissing p = liftD (D.createDirectoryIfMissing p)

-- | Ensure that a directory exists creating it and its parent directories
-- if necessary. This is just a handy shortcut:
--
-- > ensureDir = createDirIfMissing True
--
-- @since 0.3.1
ensureDir :: (MonadIO m) => Path b Dir -> m ()
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
removeDir :: (MonadIO m) => Path b Dir -> m ()
removeDir = liftD D.removeDirectory

-- | @'removeDirRecur' dir@ removes an existing directory @dir@ together
-- with its contents and sub-directories. Within this directory, symbolic
-- links are removed without affecting their targets.
removeDirRecur :: (MonadIO m) => Path b Dir -> m ()
removeDirRecur = liftD D.removeDirectoryRecursive

-- | Remove a file or directory at /path/ together with its contents and
-- subdirectories. Symbolic links are removed without affecting their
-- targets. If the path does not exist, nothing happens.
--
-- Unlike other removal functions, this function will also attempt to delete
-- files marked as read-only or otherwise made unremovable due to permissions.
-- As a result, if the removal is incomplete, the permissions or attributes on
-- the remaining files may be altered.  If there are hard links in the
-- directory, then permissions on all related hard links may be altered.
--
-- If an entry within the directory vanishes while @removePathForcibly@ is
-- running, it is silently ignored.
--
-- If an exception occurs while removing an entry, @removePathForcibly@ will
-- still try to remove as many entries as it can before failing with an
-- exception.  The first exception that it encountered is re-thrown.
--
-- @since 1.7.0
removePathForcibly :: (MonadIO m) => Path b t -> m ()
removePathForcibly = liftD D.removePathForcibly

-- | @'renameDir' old new@ changes the name of an existing directory from
--  @old@ to @new@. If the @new@ directory already exists, it is atomically
--  replaced by the @old@ directory. If the @new@ directory is neither the
--  @old@ directory nor an alias of the @old@ directory, it is removed as if
--  by 'removeDir'. A conformant implementation need not support renaming
--  directories in all situations (e.g. renaming to an existing directory, or
--  across different physical devices), but the constraints must be
--  documented.
--
--  On Win32 platforms, @renameDir@ fails if the @new@ directory already
--  exists.
--
--  The operation may fail with:
--
--  * 'HardwareFault'
--  A physical I\/O error has occurred.
--  @[EIO]@
--
--  * 'InvalidArgument'
--  Either operand is not a valid directory name.
--  @[ENAMETOOLONG, ELOOP]@
--
--  * 'isDoesNotExistError' \/ 'NoSuchThing'
--  The original directory does not exist, or there is no path to the target.
--  @[ENOENT, ENOTDIR]@
--
--  * 'isPermissionError' \/ 'PermissionDenied'
--  The process has insufficient privileges to perform the operation.
--  @[EROFS, EACCES, EPERM]@
--
--  * 'ResourceExhausted'
--  Insufficient resources are available to perform the operation.
--  @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
--
--  * 'UnsatisfiedConstraints'
--  Implementation-dependent constraints are not satisfied.
--  @[EBUSY, ENOTEMPTY, EEXIST]@
--
--  * 'UnsupportedOperation'
--  The implementation does not support renaming in this situation.
--  @[EINVAL, EXDEV]@
--
--  * 'InappropriateType'
--  Either path refers to an existing non-directory object.
--  @[ENOTDIR, EISDIR]@
renameDir ::
  (MonadIO m) =>
  -- | Old name
  Path b0 Dir ->
  -- | New name
  Path b1 Dir ->
  m ()
renameDir = liftD2 D.renameDirectory

-- | Rename a file or directory.  If the destination path already exists, it
-- is replaced atomically.  The destination path must not point to an existing
-- directory.  A conformant implementation need not support renaming files in
-- all situations (e.g. renaming across different physical devices), but the
-- constraints must be documented.
--
-- The operation may fail with:
--
-- * @HardwareFault@
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * @InvalidArgument@
-- Either operand is not a valid file name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError'
-- The original file does not exist, or there is no path to the target.
-- @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError'
-- The process has insufficient privileges to perform the operation.
-- @[EROFS, EACCES, EPERM]@
--
-- * 'System.IO.isFullError'
-- Insufficient resources are available to perform the operation.
-- @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
--
-- * @UnsatisfiedConstraints@
-- Implementation-dependent constraints are not satisfied.
-- @[EBUSY]@
--
-- * @UnsupportedOperation@
-- The implementation does not support renaming in this situation.
-- @[EXDEV]@
--
-- * @InappropriateType@
-- Either the destination path refers to an existing directory, or one of the
-- parent segments in the destination path is not a directory.
-- @[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@
--
-- @since 1.7.0
renamePath :: (MonadIO m) => Path b0 t -> Path b1 t -> m ()
renamePath = liftD2 D.renamePath

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
listDir ::
  (MonadIO m) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  m ([Path Abs Dir], [Path Abs File])
listDir path = do
  bpath <- makeAbsolute path
  (subdirs, files) <- listDirRel bpath
  return
    ( (bpath </>) <$> subdirs,
      (bpath </>) <$> files
    )

-- | The same as 'listDir' but returns relative paths.
--
-- @since 1.4.0
listDirRel ::
  (MonadIO m) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  m ([Path Rel Dir], [Path Rel File])
listDirRel path = liftIO $ do
  raw <- liftD D.getDirectoryContents path
  items <- forM (raw \\ [".", ".."]) $ \item -> do
    isDir <- liftIO (D.doesDirectoryExist $ toFilePath path F.</> item)
    if isDir
      then Left <$> parseRelDir item
      else Right <$> parseRelFile item
  return (lefts items, rights items)

-- | Similar to 'listDir', but recursively traverses every sub-directory
-- /excluding symbolic links/, and returns all files and directories found.
-- This can fail with the same exceptions as 'listDir'.
--
-- __Note__: before version /1.3.0/, this function followed symlinks.
listDirRecur ::
  (MonadIO m) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  m ([Path Abs Dir], [Path Abs File])
listDirRecur dir =
  (DList.toList *** DList.toList)
    <$> walkDirAccum (Just excludeSymlinks) writer dir
  where
    excludeSymlinks _ subdirs _ =
      WalkExclude <$> filterM isSymlink subdirs
    writer _ ds fs =
      return
        ( DList.fromList ds,
          DList.fromList fs
        )

-- | The same as 'listDirRecur' but returns paths that are relative to the
-- given directory.
--
-- @since 1.4.2
listDirRecurRel ::
  (MonadIO m) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  m ([Path Rel Dir], [Path Rel File])
listDirRecurRel dir =
  (DList.toList *** DList.toList)
    <$> walkDirAccumRel (Just excludeSymlinks) writer dir
  where
    excludeSymlinks tdir subdirs _ =
      WalkExclude <$> filterM (isSymlink . (dir </>) . (tdir </>)) subdirs
    writer tdir ds fs =
      return
        ( DList.fromList ((tdir </>) <$> ds),
          DList.fromList ((tdir </>) <$> fs)
        )

-- | Copies a directory recursively. It /does not/ follow symbolic links and
-- preserves permissions when possible. If the destination directory already
-- exists, new files and sub-directories complement its structure, possibly
-- overwriting old files if they happen to have the same name as the new
-- ones.
--
-- __Note__: before version /1.3.0/, this function followed symlinks.
--
-- __Note__: before version /1.6.0/, the function created empty directories
-- in the destination directory when the source directory contained
-- directory symlinks. The symlinked directories were not recursively
-- traversed. It also copied symlinked files creating normal regular files
-- in the target directory as the result. This was fixed in the version
-- /1.6.0/ so that the function now behaves much like the @cp@ utility, not
-- traversing symlinked directories, but recreating symlinks in the target
-- directory according to their targets in the source directory.
copyDirRecur ::
  (MonadIO m) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  m ()
copyDirRecur = copyDirRecurGen True

-- | The same as 'copyDirRecur', but it /does not/ preserve directory
-- permissions. This may be useful, for example, if the directory you want
-- to copy is “read-only”, but you want your copy to be editable.
--
-- @since 1.1.0
--
-- __Note__: before version /1.3.0/, this function followed symlinks.
--
-- __Note__: before version /1.6.0/, the function created empty directories
-- in the destination directory when the source directory contained
-- directory symlinks. The symlinked directories were not recursively
-- traversed. It also copied symlinked files creating normal regular files
-- in the target directory as the result. This was fixed in the version
-- /1.6.0/ so that the function now behaves much like the @cp@ utility, not
-- traversing symlinked directories, but recreating symlinks in the target
-- directory according to their targets in the source directory.
copyDirRecur' ::
  (MonadIO m) =>
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  m ()
copyDirRecur' = copyDirRecurGen False

-- | Generic version of 'copyDirRecur'. The first argument controls whether
-- to preserve directory permissions or not. /Does not/ follow symbolic
-- links. Internal function.
copyDirRecurGen ::
  (MonadIO m) =>
  -- | Should we preserve directory permissions?
  Bool ->
  -- | Source
  Path b0 Dir ->
  -- | Destination
  Path b1 Dir ->
  m ()
copyDirRecurGen preserveDirPermissions src dest = liftIO $ do
  bsrc <- makeAbsolute src
  bdest <- makeAbsolute dest
  (dirs, files) <- listDirRecur bsrc
  let swapParent ::
        Path Abs Dir ->
        Path Abs Dir ->
        Path Abs t ->
        IO (Path Abs t)
      swapParent old new path =
        (new </>)
          <$> stripProperPrefix old path
  ensureDir bdest
  copyPermissionsIOs <- forM dirs $ \srcDir -> do
    destDir <- swapParent bsrc bdest srcDir
    dirIsSymlink <- isSymlink srcDir
    if dirIsSymlink
      then do
        target <- getSymlinkTarget srcDir
        D.createDirectoryLink target $
          toFilePath' destDir
      else ensureDir destDir
    pure $ ignoringIOErrors (copyPermissions srcDir destDir)
  forM_ files $ \srcFile -> do
    destFile <- swapParent bsrc bdest srcFile
    fileIsSymlink <- isSymlink srcFile
    if fileIsSymlink
      then do
        target <- getSymlinkTarget srcFile
        D.createFileLink target (toFilePath destFile)
      else copyFile srcFile destFile
  when preserveDirPermissions $ do
    ignoringIOErrors (copyPermissions bsrc bdest)
    sequence_ copyPermissionsIOs

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
-- - In the context of the parent dir, decide which subdirs to descend into.
-- - In the context of the subdir, decide whether to traverse the subdir or not.
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
  = -- | Finish the entire walk altogether
    WalkFinish
  | -- | List of sub-directories to exclude from
    -- descending
    WalkExclude [Path b Dir]
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
walkDir ::
  (MonadIO m) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (WalkAction Abs)) ->
  -- | Directory where traversal begins
  Path b Dir ->
  m ()
walkDir handler topdir =
  void $
    makeAbsolute topdir >>= walkAvoidLoop S.empty
  where
    walkAvoidLoop traversed curdir = do
      mRes <- checkLoop traversed curdir
      case mRes of
        Nothing -> return $ Just ()
        Just traversed' -> walktree traversed' curdir
    walktree traversed curdir = do
      (subdirs, files) <- listDir curdir
      action <- handler curdir subdirs files
      case action of
        WalkFinish -> return Nothing
        WalkExclude xdirs ->
          case subdirs \\ xdirs of
            [] -> return $ Just ()
            ds ->
              runMaybeT $
                mapM_
                  (MaybeT . walkAvoidLoop traversed)
                  ds
    checkLoop traversed dir = do
      st <- liftIO $ P.getFileStatus (fromAbsDir dir)
      let ufid = (P.deviceID st, P.fileID st)
      return $
        if S.member ufid traversed
          then Nothing
          else Just (S.insert ufid traversed)

-- | The same as 'walkDir' but uses relative paths. The handler is given
-- @dir@, directory relative to the directory where traversal begins.
-- Sub-directories and files are relative to @dir@.
--
-- @since 1.4.2
walkDirRel ::
  (MonadIO m) =>
  -- | Handler (@dir -> subdirs -> files -> 'WalkAction'@)
  ( Path Rel Dir ->
    [Path Rel Dir] ->
    [Path Rel File] ->
    m (WalkAction Rel)
  ) ->
  -- | Directory where traversal begins
  Path b Dir ->
  m ()
walkDirRel handler topdir' = do
  topdir <- makeAbsolute topdir'
  let walkAvoidLoop traversed curdir = do
        mRes <- checkLoop traversed (topdir </> curdir)
        case mRes of
          Nothing -> return $ Just ()
          Just traversed' -> walktree traversed' curdir
      walktree traversed curdir = do
        (subdirs, files) <- listDirRel (topdir </> curdir)
        action <- handler curdir subdirs files
        case action of
          WalkFinish -> return Nothing
          WalkExclude xdirs ->
            case subdirs \\ xdirs of
              [] -> return $ Just ()
              ds ->
                runMaybeT $
                  mapM_
                    (MaybeT . walkAvoidLoop traversed)
                    ((curdir </>) <$> ds)
      checkLoop traversed dir = do
        st <- liftIO $ P.getFileStatus (fromAbsDir dir)
        let ufid = (P.deviceID st, P.fileID st)
        return $
          if S.member ufid traversed
            then Nothing
            else Just (S.insert ufid traversed)
  void (walkAvoidLoop S.empty $(mkRelDir "."))

-- | Similar to 'walkDir' but accepts a 'Monoid'-returning output writer as
-- well. Values returned by the output writer invocations are accumulated
-- and returned.
--
-- Both, the descend handler as well as the output writer can be used for
-- side effects but keep in mind that the output writer runs before the
-- descend handler.
--
-- @since 1.2.0
walkDirAccum ::
  (MonadIO m, Monoid o) =>
  -- | Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
  -- descend the whole tree if omitted
  Maybe
    (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m (WalkAction Abs)) ->
  -- | Output writer (@dir -> subdirs -> files -> o@)
  (Path Abs Dir -> [Path Abs Dir] -> [Path Abs File] -> m o) ->
  -- | Directory where traversal begins
  Path b Dir ->
  -- | Accumulation of outputs generated by the output writer invocations
  m o
walkDirAccum = walkDirAccumWith walkDir

-- | The same as 'walkDirAccum' but uses relative paths. The handler and
-- writer are given @dir@, directory relative to the directory where
-- traversal begins. Sub-directories and files are relative to @dir@.
--
-- @since 1.4.2
walkDirAccumRel ::
  (MonadIO m, Monoid o) =>
  -- | Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
  -- descend the whole tree if omitted
  Maybe
    (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (WalkAction Rel)) ->
  -- | Output writer (@dir -> subdirs -> files -> o@)
  (Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m o) ->
  -- | Directory where traversal begins
  Path b Dir ->
  -- | Accumulation of outputs generated by the output writer invocations
  m o
walkDirAccumRel = walkDirAccumWith walkDirRel

-- | Non-public helper function for defining accumulating walking actions.
walkDirAccumWith ::
  (MonadIO m, Monoid o) =>
  -- | The walk function we use
  ( ( Path a Dir ->
      [Path a Dir] ->
      [Path a File] ->
      WriterT o m (WalkAction a)
    ) ->
    Path b Dir ->
    WriterT o m ()
  ) ->
  -- | Descend handler (@dir -> subdirs -> files -> 'WalkAction'@),
  -- descend the whole tree if omitted
  Maybe (Path a Dir -> [Path a Dir] -> [Path a File] -> m (WalkAction a)) ->
  -- | Output writer (@dir -> subdirs -> files -> o@)
  (Path a Dir -> [Path a Dir] -> [Path a File] -> m o) ->
  -- | Directory where traversal begins
  Path b Dir ->
  -- | Accumulation of outputs generated by the output writer invocations
  m o
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
getCurrentDir :: (MonadIO m) => m (Path Abs Dir)
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
setCurrentDir :: (MonadIO m) => Path b Dir -> m ()
setCurrentDir = liftD D.setCurrentDirectory

-- | Run an 'IO' action with the given working directory and restore the
-- original working directory afterwards, even if the given action fails due
-- to an exception.
--
-- The operation may fail with the same exceptions as 'getCurrentDir' and
-- 'setCurrentDir'.
withCurrentDir ::
  (MonadIO m, MonadMask m) =>
  -- | Directory to execute in
  Path b Dir ->
  -- | Action to be executed
  m a ->
  m a
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
getHomeDir :: (MonadIO m) => m (Path Abs Dir)
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
getAppUserDataDir ::
  (MonadIO m) =>
  -- | Name of application (used in path construction)
  String ->
  m (Path Abs Dir)
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
getUserDocsDir :: (MonadIO m) => m (Path Abs Dir)
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
getTempDir :: (MonadIO m) => m (Path Abs Dir)
getTempDir = liftIO D.getTemporaryDirectory >>= resolveDir'

-- | Obtain the paths to special directories for storing user-specific
-- application data, configuration, and cache files, conforming to the
-- <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
-- Compared with 'getAppUserDataDir', this function provides a more
-- fine-grained hierarchy as well as greater flexibility for the user.
--
-- It also works on Windows, although in that case 'D.XdgData' and
-- 'D.XdgConfig' will map to the same directory.
--
-- Note: The directory may not actually exist, in which case you would need
-- to create it with file mode @700@ (i.e. only accessible by the owner).
--
-- Note also: this is a piece of conditional API, only available if
-- @directory-1.2.3.0@ or later is used.
--
-- @since 1.2.1
getXdgDir ::
  (MonadIO m) =>
  -- | Which special directory
  D.XdgDirectory ->
  -- | A relative path that is appended to the path; if 'Nothing', the
  -- base path is returned
  Maybe (Path Rel Dir) ->
  m (Path Abs Dir)
getXdgDir xdgDir suffix =
  liftIO $ (D.getXdgDirectory xdgDir $ maybe "" toFilePath suffix) >>= parseAbsDir

-- | Similar to 'getXdgDir' but retrieves the entire list of XDG
-- directories.
--
-- On Windows, 'D.XdgDataDirs' and 'D.XdgConfigDirs' usually map to the same
-- list of directories unless overridden.
--
-- Refer to the docs of 'D.XdgDirectoryList' for more details.
--
-- @since 1.5.0
getXdgDirList ::
  (MonadIO m) =>
  -- | Which special directory list
  D.XdgDirectoryList ->
  m [Path Abs Dir]
getXdgDirList xdgDirList =
  liftIO (D.getXdgDirectoryList xdgDirList >>= mapM parseAbsDir)

----------------------------------------------------------------------------
-- Path transformation

-- | Class of things ('Path's) that can be canonicalized, made absolute, and
-- made relative to a some base directory.
class AnyPath path where
  -- | Type of absolute version of the given @path@.
  type AbsPath path :: Type

  -- | Type of relative version of the given @path@.
  type RelPath path :: Type

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
  canonicalizePath ::
    (MonadIO m) =>
    path ->
    m (AbsPath path)

  -- | Make a path absolute by prepending the current directory (if it isn't
  -- already absolute) and applying 'F.normalise' to the result.
  --
  -- If the path is already absolute, the operation never fails. Otherwise,
  -- the operation may fail with the same exceptions as 'getCurrentDir'.
  makeAbsolute ::
    (MonadIO m) =>
    path ->
    m (AbsPath path)

  -- | Make a path relative to a given directory.
  --
  -- @since 0.3.0
  makeRelative ::
    (MonadThrow m) =>
    -- | Base directory
    Path Abs Dir ->
    -- | Path that will be made relative to base directory
    path ->
    m (RelPath path)

  -- | Make a path relative to current working directory.
  --
  -- @since 0.3.0
  makeRelativeToCurrentDir ::
    (MonadIO m) =>
    path ->
    m (RelPath path)

instance AnyPath (Path b File) where
  type AbsPath (Path b File) = Path Abs File
  type RelPath (Path b File) = Path Rel File

  canonicalizePath = liftD $ D.canonicalizePath >=> parseAbsFile
  makeAbsolute = liftD $ D.makeAbsolute >=> parseAbsFile
  makeRelative b p = parseRelFile (F.makeRelative (toFilePath b) (toFilePath p))
  makeRelativeToCurrentDir p = liftIO $ getCurrentDir >>= flip makeRelative p

instance AnyPath (Path b Dir) where
  type AbsPath (Path b Dir) = Path Abs Dir
  type RelPath (Path b Dir) = Path Rel Dir

  canonicalizePath = liftD D.canonicalizePath >=> liftIO . parseAbsDir
  makeAbsolute = liftD D.makeAbsolute >=> liftIO . parseAbsDir
  makeRelative b p = parseRelDir (F.makeRelative (toFilePath b) (toFilePath p))
  makeRelativeToCurrentDir p = liftIO $ getCurrentDir >>= flip makeRelative p

-- | @since 1.8.0
instance AnyPath (SomeBase File) where
  type AbsPath (SomeBase File) = Path Abs File
  type RelPath (SomeBase File) = Path Rel File

  canonicalizePath s = case s of
    Abs a -> canonicalizePath a
    Rel a -> canonicalizePath a

  makeAbsolute s = case s of
    Abs a -> makeAbsolute a
    Rel a -> makeAbsolute a

  makeRelative r s = case s of
    Abs a -> makeRelative r a
    Rel a -> makeRelative r a

  makeRelativeToCurrentDir s = case s of
    Abs a -> makeRelativeToCurrentDir a
    Rel a -> makeRelativeToCurrentDir a

-- | @since 1.8.0
instance AnyPath (SomeBase Dir) where
  type AbsPath (SomeBase Dir) = Path Abs Dir
  type RelPath (SomeBase Dir) = Path Rel Dir

  canonicalizePath s = case s of
    Abs a -> canonicalizePath a
    Rel a -> canonicalizePath a

  makeAbsolute s = case s of
    Abs a -> makeAbsolute a
    Rel a -> makeAbsolute a

  makeRelative r s = case s of
    Abs a -> makeRelative r a
    Rel a -> makeRelative r a

  makeRelativeToCurrentDir s = case s of
    Abs a -> makeRelativeToCurrentDir a
    Rel a -> makeRelativeToCurrentDir a

-- | Append stringly-typed path to an absolute path and then canonicalize
-- it.
--
-- @since 0.3.0
resolveFile ::
  (MonadIO m) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  FilePath ->
  m (Path Abs File)
resolveFile b p = liftIO $ D.canonicalizePath (toFilePath b F.</> p) >>= parseAbsFile

-- | The same as 'resolveFile', but uses current working directory.
--
-- @since 0.3.0
resolveFile' ::
  (MonadIO m) =>
  -- | Path to resolve
  FilePath ->
  m (Path Abs File)
resolveFile' p = getCurrentDir >>= flip resolveFile p

-- | The same as 'resolveFile', but for directories.
--
-- @since 0.3.0
resolveDir ::
  (MonadIO m) =>
  -- | Base directory
  Path Abs Dir ->
  -- | Path to resolve
  FilePath ->
  m (Path Abs Dir)
resolveDir b p = liftIO $ D.canonicalizePath (toFilePath b F.</> p) >>= parseAbsDir

-- | The same as 'resolveDir', but uses current working directory.
--
-- @since 0.3.0
resolveDir' ::
  (MonadIO m) =>
  -- | Path to resolve
  FilePath ->
  m (Path Abs Dir)
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
removeFile :: (MonadIO m) => Path b File -> m ()
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
renameFile ::
  (MonadIO m) =>
  -- | Original location
  Path b0 File ->
  -- | New location
  Path b1 File ->
  m ()
renameFile = liftD2 D.renameFile

-- | @'copyFile' old new@ copies the existing file from @old@ to @new@. If
-- the @new@ file already exists, it is atomically replaced by the @old@
-- file. Neither path may refer to an existing directory. The permissions of
-- @old@ are copied to @new@, if possible.
copyFile ::
  (MonadIO m) =>
  -- | Original location
  Path b0 File ->
  -- | Where to put copy
  Path b1 File ->
  m ()
copyFile = liftD2 D.copyFile

-- | Obtain the size of a file in bytes.
--
-- @since 1.7.0
getFileSize :: (MonadIO m) => Path b File -> m Integer
getFileSize = liftD D.getFileSize

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
findExecutable ::
  (MonadIO m) =>
  -- | Executable file name
  Path Rel File ->
  -- | Path to found executable
  m (Maybe (Path Abs File))
findExecutable = fmap (>>= parseAbsFile) . liftD D.findExecutable

-- | Search through the given set of directories for the given file.
findFile ::
  (MonadIO m) =>
  -- | Set of directories to search in
  [Path b Dir] ->
  -- | Filename of interest
  Path Rel File ->
  -- | Absolute path to file (if found)
  m (Maybe (Path Abs File))
findFile [] _ = return Nothing
findFile (d : ds) file = do
  bfile <- (</> file) <$> makeAbsolute d
  exist <- doesFileExist bfile
  if exist
    then return (Just bfile)
    else findFile ds file

-- | Search through the given set of directories for the given file and
-- return a list of paths where the given file exists.
findFiles ::
  (MonadIO m) =>
  -- | Set of directories to search in
  [Path b Dir] ->
  -- | Filename of interest
  Path Rel File ->
  -- | Absolute paths to all found files
  m [Path Abs File]
findFiles = findFilesWith (const (return True))

-- | Search through the given set of directories for the given file and with
-- the given property (usually permissions) and return a list of paths where
-- the given file exists and has the property.
findFilesWith ::
  (MonadIO m) =>
  -- | How to test the files
  (Path Abs File -> m Bool) ->
  -- | Set of directories to search in
  [Path b Dir] ->
  -- | Filename of interest
  Path Rel File ->
  -- | Absolute paths to all found files
  m [Path Abs File]
findFilesWith _ [] _ = return []
findFilesWith f (d : ds) file = do
  bfile <- (</> file) <$> makeAbsolute d
  exist <- doesFileExist bfile
  b <- if exist then f bfile else return False
  if b
    then (bfile :) <$> findFilesWith f ds file
    else findFilesWith f ds file

----------------------------------------------------------------------------
-- Symbolic links

-- | Create a /file/ symbolic link. The target path can be either absolute
-- or relative and need not refer to an existing file. The order of
-- arguments follows the POSIX convention.
--
-- To remove an existing file symbolic link, use 'removeFile'.
--
-- Although the distinction between /file/ symbolic links and /directory/
-- symbolic links does not exist on POSIX systems, on Windows this is an
-- intrinsic property of every symbolic link and cannot be changed without
-- recreating the link. A file symbolic link that actually points to a
-- directory will fail to dereference and vice versa. Moreover, creating
-- symbolic links on Windows may require privileges unavailable to users
-- outside the Administrators group. Portable programs that use symbolic
-- links should take both into consideration.
--
-- On Windows, the function is implemented using @CreateSymbolicLink@. Since
-- 1.3.3.0, the @SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE@ flag is
-- included if supported by the operating system. On POSIX, the function
-- uses @symlink@ and is therefore atomic.
--
-- Windows-specific errors: This operation may fail with
-- 'System.IO.Error.permissionErrorType' if the user lacks the privileges to
-- create symbolic links. It may also fail with
-- 'System.IO.Error.illegalOperationErrorType' if the file system does not
-- support symbolic links.
--
-- @since 1.5.0
createFileLink ::
  (MonadIO m) =>
  -- | Path to the target file
  Path b0 File ->
  -- | Path to the link to be created
  Path b1 File ->
  m ()
createFileLink = liftD2 D.createFileLink

-- | Create a /directory/ symbolic link. The target path can be either
-- absolute or relative and need not refer to an existing directory. The
-- order of arguments follows the POSIX convention.
--
-- To remove an existing directory symbolic link, use 'removeDirLink'.
--
-- Although the distinction between /file/ symbolic links and /directory/
-- symbolic links does not exist on POSIX systems, on Windows this is an
-- intrinsic property of every symbolic link and cannot be changed without
-- recreating the link. A file symbolic link that actually points to a
-- directory will fail to dereference and vice versa. Moreover, creating
-- symbolic links on Windows may require privileges unavailable to users
-- outside the Administrators group. Portable programs that use symbolic
-- links should take both into consideration.
--
-- On Windows, the function is implemented using @CreateSymbolicLink@ with
-- @SYMBOLIC_LINK_FLAG_DIRECTORY@. Since 1.3.3.0, the
-- @SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE@ flag is also included if
-- supported by the operating system. On POSIX, this is an alias for
-- 'createFileLink' and is therefore atomic.
--
-- Windows-specific errors: This operation may fail with
-- 'System.IO.Error.permissionErrorType' if the user lacks the privileges to
-- create symbolic links. It may also fail with
-- 'System.IO.Error.illegalOperationErrorType' if the file system does not
-- support symbolic links.
--
-- @since 1.5.0
createDirLink ::
  (MonadIO m) =>
  -- | Path to the target directory
  Path b0 Dir ->
  -- | Path to the link to be created
  Path b1 Dir ->
  m ()
createDirLink target' dest' = do
  let target = toFilePath target'
      dest = toFilePath' dest'
  liftIO $ D.createDirectoryLink target dest

-- | Remove an existing /directory/ symbolic link.
--
-- On Windows, this is an alias for 'removeDir'. On POSIX systems, this is
-- an alias for 'removeFile'.
--
-- See also: 'removeFile', which can remove an existing /file/ symbolic link.
--
-- @since 1.5.0
removeDirLink ::
  (MonadIO m) =>
  -- | Path to the link to be removed
  Path b Dir ->
  m ()
removeDirLink = liftD D.removeDirectoryLink

-- | Retrieve the target path of either a file or directory symbolic link.
-- The returned path may not exist, and may not even be a valid path.
--
-- On Windows systems, this calls @DeviceIoControl@ with
-- @FSCTL_GET_REPARSE_POINT@. In addition to symbolic links, the function
-- also works on junction points. On POSIX systems, this calls @readlink@.
--
-- Windows-specific errors: This operation may fail with
-- 'System.IO.Error.illegalOperationErrorType' if the file system does not
-- support symbolic links.
--
-- @since 1.5.0
getSymlinkTarget ::
  (MonadIO m) =>
  -- | Symlink path
  Path b t ->
  m FilePath
getSymlinkTarget = liftD D.getSymbolicLinkTarget

-- | Check whether the path refers to a symbolic link.  An exception is thrown
-- if the path does not exist or is inaccessible.
--
-- On Windows, this checks for @FILE_ATTRIBUTE_REPARSE_POINT@.  In addition to
-- symbolic links, the function also returns true on junction points.  On
-- POSIX systems, this checks for @S_IFLNK@.
--
-- @since 1.5.0

-- | Check if the given path is a symbolic link.
--
-- @since 1.3.0
isSymlink :: (MonadIO m) => Path b t -> m Bool
isSymlink = liftD D.pathIsSymbolicLink

----------------------------------------------------------------------------
-- Temporary files and directories

-- | Use a temporary file that doesn't already exist.
--
-- Creates a new temporary file inside the given directory, making use of
-- the template. The temporary file is deleted after use.
--
-- @since 0.2.0
withTempFile ::
  (MonadIO m, MonadMask m) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  String ->
  -- | Callback that can use the file
  (Path Abs File -> Handle -> m a) ->
  m a
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
withTempDir ::
  (MonadIO m, MonadMask m) =>
  -- | Directory to create the file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  String ->
  -- | Callback that can use the directory
  (Path Abs Dir -> m a) ->
  m a
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
withSystemTempFile ::
  (MonadIO m, MonadMask m) =>
  -- | File name template, see 'openTempFile'
  String ->
  -- | Callback that can use the file
  (Path Abs File -> Handle -> m a) ->
  m a
withSystemTempFile t action =
  getTempDir >>= \path ->
    withTempFile path t action

-- | Create and use a temporary directory in the system standard temporary
-- directory.
--
-- Behaves exactly the same as 'withTempDir', except that the parent
-- temporary directory will be that returned by 'getTempDir'.
--
-- @since 0.2.0
withSystemTempDir ::
  (MonadIO m, MonadMask m) =>
  -- | Directory name template, see 'openTempFile'
  String ->
  -- | Callback that can use the directory
  (Path Abs Dir -> m a) ->
  m a
withSystemTempDir t action =
  getTempDir >>= \path ->
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
openTempFile ::
  (MonadIO m) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template; if the template is "foo.ext" then the created
  -- file will be @\"fooXXX.ext\"@ where @XXX@ is some random number
  String ->
  -- | Name of created file and its 'Handle'
  m (Path Abs File, Handle)
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
openBinaryTempFile ::
  (MonadIO m) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | File name template, see 'openTempFile'
  String ->
  -- | Name of created file and its 'Handle'
  m (Path Abs File, Handle)
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
createTempDir ::
  (MonadIO m) =>
  -- | Directory to create file in
  Path b Dir ->
  -- | Directory name template, see 'openTempFile'
  String ->
  -- | Name of created temporary directory
  m (Path Abs Dir)
createTempDir path t =
  liftIO $
    makeAbsolute path >>= \apath ->
      liftD2' T.createTempDirectory apath t >>= parseAbsDir

----------------------------------------------------------------------------
-- Existence tests

-- | Test whether the given path points to an existing filesystem object. If
-- the user lacks necessary permissions to search the parent directories,
-- this function may return false even if the file does actually exist.
--
-- @since 1.7.0
doesPathExist :: (MonadIO m) => Path b t -> m Bool
doesPathExist = liftD D.doesPathExist

-- | The operation 'doesFileExist' returns 'True' if the argument file
-- exists and is not a directory, and 'False' otherwise.
doesFileExist :: (MonadIO m) => Path b File -> m Bool
doesFileExist = liftD D.doesFileExist

-- | The operation 'doesDirExist' returns 'True' if the argument file exists
-- and is either a directory or a symbolic link to a directory, and 'False'
-- otherwise.
doesDirExist :: (MonadIO m) => Path b Dir -> m Bool
doesDirExist = liftD D.doesDirectoryExist

-- | Check if there is a file or directory on specified path.
isLocationOccupied :: (MonadIO m) => Path b t -> m Bool
isLocationOccupied path = do
  let fp = toFilePath path
  file <- liftIO (D.doesFileExist fp)
  dir <- liftIO (D.doesDirectoryExist fp)
  return (file || dir)

-- | If argument of the function throws a
-- 'System.IO.Error.doesNotExistErrorType', 'Nothing' is returned (other
-- exceptions propagate). Otherwise the result is returned inside a 'Just'.
--
-- @since 0.3.0
forgivingAbsence :: (MonadIO m, MonadCatch m) => m a -> m (Maybe a)
forgivingAbsence f =
  catchIf
    isDoesNotExistError
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
getPermissions :: (MonadIO m) => Path b t -> m D.Permissions
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
setPermissions :: (MonadIO m) => Path b t -> D.Permissions -> m ()
setPermissions = liftD2' D.setPermissions

-- | Set permissions for the object found on second given path so they match
-- permissions of the object on the first path.
copyPermissions ::
  (MonadIO m) =>
  -- | From where to copy
  Path b0 t0 ->
  -- | What to modify
  Path b1 t1 ->
  m ()
copyPermissions = liftD2 D.copyPermissions

----------------------------------------------------------------------------
-- Timestamps

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
getAccessTime :: (MonadIO m) => Path b t -> m UTCTime
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
setAccessTime :: (MonadIO m) => Path b t -> UTCTime -> m ()
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
setModificationTime :: (MonadIO m) => Path b t -> UTCTime -> m ()
setModificationTime = liftD2' D.setModificationTime

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
getModificationTime :: (MonadIO m) => Path b t -> m UTCTime
getModificationTime = liftD D.getModificationTime

----------------------------------------------------------------------------
-- Helpers

-- | Lift an action in 'IO' that takes 'FilePath' into an action in slightly
-- more abstract monad that takes 'Path'.
liftD ::
  (MonadIO m) =>
  -- | Original action
  (FilePath -> IO a) ->
  -- | 'Path' argument
  Path b t ->
  -- | Lifted action
  m a
liftD m = liftIO . m . toFilePath'
{-# INLINE liftD #-}

-- | Similar to 'liftD' but for functions with arity 2.
liftD2 ::
  (MonadIO m) =>
  -- | Original action
  (FilePath -> FilePath -> IO a) ->
  -- | First 'Path' argument
  Path b0 t0 ->
  -- | Second 'Path' argument
  Path b1 t1 ->
  m a
liftD2 m a b = liftIO $ m (toFilePath' a) (toFilePath' b)
{-# INLINE liftD2 #-}

-- | Similar to 'liftD2', but allows us to pass second argument of arbitrary
-- type.
liftD2' ::
  (MonadIO m) =>
  -- | Original action
  (FilePath -> v -> IO a) ->
  -- | First 'Path' argument
  Path b t ->
  -- | Second argument
  v ->
  m a
liftD2' m a v = liftIO $ m (toFilePath' a) v
{-# INLINE liftD2' #-}

-- | Like 'toFilePath', but also drops the trailing path separator.
toFilePath' :: Path b t -> FilePath
toFilePath' = F.dropTrailingPathSeparator . toFilePath

-- | Perform an action ignoring IO exceptions it may throw.
ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `catch` handler
  where
    handler :: (Monad m) => IOError -> m ()
    handler = const (return ())
