-- |
-- Module      :  Path.IO
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides interface to "System.Directory" convenient for users
-- of "Path" module. It also implements commonly used primitives like
-- recursive scanning of copying of directories.

{-# LANGUAGE CPP #-}

module Path.IO
  ( -- * Actions on directories
    createDir
  , createDirIfMissing
  , removeDir
  , removeDirRecur
  , renameDir
  , listDir
  , listDirRecur
  , copyDirRecur
    -- ** Current working directory
  , getCurrentDir
  , setCurrentDir
  , withCurrentDir
    -- * Pre-defined directories
  , getHomeDir
  , getAppUserDataDir
  , getUserDocsDir
  , getTempDir
    -- * Path transformation
  , canonicalizePath
  , makeAbsolute
    -- * Actions on files
  , removeFile
  , renameFile
  , copyFile
  , findExecutable
  , findFile
  , findFiles
  , findFilesWith
    -- * Existence tests
  , doesFileExist
  , doesDirExist
  , isLocationOccupied
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

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO (..))
import Data.Time (UTCTime)
import Path
import qualified System.Directory as D

----------------------------------------------------------------------------
-- Actions on directories

-- | See 'D.createDirectory'.

createDir :: MonadIO m => Path b Dir -> m ()
createDir = liftD D.createDirectory

-- | See 'D.createDirectoryIfMissing'.

createDirIfMissing :: MonadIO m
  => Bool              -- ^ Create its parents too?
  -> Path b Dir        -- ^ The path to the directory you want to make
  -> m ()
createDirIfMissing p = liftD (D.createDirectoryIfMissing p)

-- | See 'D.removeDirectory'.

removeDir :: MonadIO m => Path b Dir -> m ()
removeDir = liftD D.removeDirectory

-- | See 'D.removeDirectoryRecursive'.

removeDirRecur :: MonadIO m => Path b Dir -> m ()
removeDirRecur = liftD D.removeDirectoryRecursive

-- | See 'D.renameDirectory'.

renameDir :: MonadIO m
  => Path b1 Dir       -- ^ Source
  -> Path b2 Dir       -- ^ Destination
  -> m ()
renameDir = liftD2 D.renameDirectory

-- | @'listDir' dir@ returns a list of /all/ entries in @dir@ without the
-- special entries (@.@ and @..@).
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
  => Path b Dir
  -> m ([Path Abs Dir], [Path Abs File])
listDir path = undefined
  -- (filter f) <$> (getDirectoryContents path)
  -- where f filename = filename /= "." && filename /= ".."

-- | FIXME

listDirRecur :: MonadIO m
  => Path b Dir
  -> m [Path Abs File]
listDirRecur = undefined

-- | Copy directory recursively. FIXME add better description

copyDirRecur :: MonadIO m
  => Path b0 Dir
  -> Path b1 Dir
  -> m ()
copyDirRecur = undefined

----------------------------------------------------------------------------
-- Current working directory

-- | See 'D.getCurrentDirectory'.

getCurrentDir :: (MonadIO m, MonadThrow m) => m (Path Abs Dir)
getCurrentDir = liftIO D.getCurrentDirectory >>= parseAbsDir

-- | See 'D.setCurrentDirectory'

setCurrentDir :: MonadIO m => Path b Dir -> m ()
setCurrentDir = liftD D.setCurrentDirectory

-- | Run an 'IO' action with the given working directory and restore the
-- original working directory afterwards, even if the given action fails due
-- to an exception.
--
-- The operation may fail with the same exceptions as 'getCurrentDirectory'
-- and 'setCurrentDirectory'.

withCurrentDir :: (MonadIO m, MonadMask m)
  => Path b Dir        -- ^ Directory to execute in
  -> m a               -- ^ Action to be executed
  -> m a
withCurrentDir dir action =
  bracket getCurrentDir setCurrentDir $ const (setCurrentDir dir >> action)

----------------------------------------------------------------------------
-- Pre-defined directories

-- | See 'D.getHomeDirectory'.

getHomeDir :: (MonadIO m, MonadThrow m) => m (Path Abs Dir)
getHomeDir = liftIO D.getHomeDirectory >>= parseAbsDir

-- | See 'D.getAppUserDataDirectory'.

getAppUserDataDir :: (MonadIO m, MonadThrow m)
  => Path File Dir     -- ^ A relative path that is appended to the path
  -> m (Path Abs Dir)
getAppUserDataDir = (>>= parseAbsDir) . (liftD D.getAppUserDataDirectory)

-- | See 'D.getUserDocumentsDirectory'.

getUserDocsDir :: (MonadIO m, MonadThrow m) => m (Path Abs Dir)
getUserDocsDir = liftIO D.getUserDocumentsDirectory >>= parseAbsDir

-- | See 'D.getTemporaryDirectory'.

getTempDir :: (MonadIO m, MonadThrow m) => m (Path Abs Dir)
getTempDir = liftIO D.getTemporaryDirectory >>= parseAbsDir

----------------------------------------------------------------------------
-- Path transformation

-- | See 'D.canonicalizePath'.

canonicalizePath :: (MonadIO m, MonadThrow m)
  => Path b t
  -> m (Path Abs t)
canonicalizePath = undefined -- TODO

-- | See 'D.makeAbsolute'.

makeAbsolute :: (MonadIO m, MonadThrow m)
  => Path b t
  -> m (Path Abs t)
makeAbsolute = undefined -- TODO

----------------------------------------------------------------------------
-- Actions on files

-- | See 'D.removeFile'.

removeFile :: MonadIO m => Path b File -> m ()
removeFile = liftD D.removeFile

-- | See 'D.renameFile'.

renameFile :: MonadIO m
  => Path b File       -- ^ Original location
  -> Path b File       -- ^ New location
  -> m ()
renameFile = liftD2 D.renameFile

-- | See 'D.copyFile'.

copyFile :: MonadIO m
  => Path b File       -- ^ Original location
  -> Path b File       -- ^ Where to put copy
  -> m ()
copyFile = liftD2 D.copyFile

-- | See 'D.findExecutable'.

findExecutable :: MonadIO m
  => Path Rel File     -- ^ Executable file name
  -> m (Maybe (Path Abs File)) -- ^ Path to found executable
findExecutable = liftM (>>= parseAbsFile) . liftD D.findExecutable

-- | See 'D.findFile'.

findFile :: MonadIO m
  => [Path b Dir]
  -> Path Rel File
  -> m (Maybe (Path Abs File))
findFile dirs = liftM (>>= parseAbsFile)
  . liftD (D.findFile (toFilePath <$> dirs))

-- | See 'D.findFiles'.

findFiles :: (MonadIO m, MonadThrow m)
  => [Path b Dir]
  -> Path Rel File
  -> m [Path Abs File]
findFiles dirs file = liftD (D.findFiles (toFilePath <$> dirs)) file
  >>= mapM parseAbsFile

-- | See 'D.findFilesWith'.

findFilesWith :: (MonadIO m, MonadThrow m)
  => (Path Abs File -> m Bool)
  -> [Path b Dir]
  -> Path Rel File
  -> m [Path Abs File]
findFilesWith p' dirs file = undefined -- TODO finish it later

----------------------------------------------------------------------------
-- Existence tests

-- | See 'D.doesFileExist'.

doesFileExist :: MonadIO m => Path b File -> m Bool
doesFileExist = liftD D.doesFileExist

-- | See 'D.doesDirectoryExist'.

doesDirExist :: MonadIO m => Path b Dir -> m Bool
doesDirExist = liftD D.doesDirectoryExist

-- | Check if there is a file or directory on specified path.

isLocationOccupied :: MonadIO m => Path b t -> m Bool
isLocationOccupied p = undefined -- FIXME how to handle this elegantly?
  -- liftM2 (||) (doesFileExist p) (doesDirExist p)

----------------------------------------------------------------------------
-- Permissions

-- | See 'D.getPermissions'.

getPermissions :: MonadIO m => Path b t -> m D.Permissions
getPermissions = liftD D.getPermissions

-- | See 'D.setPermissions'.

setPermissions :: MonadIO m => Path b t -> D.Permissions -> m ()
setPermissions = liftD2' D.setPermissions

-- | See 'D.copyPermissions'.

copyPermissions :: MonadIO m
  => Path b0 t0        -- ^ From where to copy
  -> Path b1 t1        -- ^ What to modify
  -> m ()
copyPermissions = liftD2 D.copyPermissions

----------------------------------------------------------------------------
-- Timestamps

#if MIN_VERSION_directory(1,2,3)

-- | See 'D.getAccessTime'.

getAccessTime :: MonadIO m => Path b t -> m UTCTime
getAccessTime = liftD D.getAccessTime

-- | See 'D.setAccessTime'.

setAccessTime :: MonadIO m => Path b t -> UTCTime -> m ()
setAccessTime = liftD2' D.setAccessTime

-- | See 'D.setModificationTime'.

setModificationTime :: MonadIO m => Path b t -> UTCTime -> m ()
setModificationTime = liftD2' D.setAccessTime

#endif

-- | See 'D.getModificationTime'.

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

-- | Similar to 'liftD' for functions with arity 2.

liftD2 :: MonadIO m
  => (FilePath -> FilePath -> IO a) -- ^ Original action
  -> Path b0 t0        -- ^ First 'Path' argument
  -> Path b1 t1        -- ^ Second 'Path' argument
  -> m a
liftD2 m a b = liftIO $ m (toFilePath a) (toFilePath b)

-- | Similar to 'liftD2', but allows to pass second argument.

liftD2' :: MonadIO m
  => (FilePath -> v -> IO a) -- ^ Original action
  -> Path b0 t0        -- ^ First 'Path' argument
  -> v                 -- ^ Second argument
  -> m a
liftD2' m a v = liftIO $ m (toFilePath a) v
