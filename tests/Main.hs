{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (sort)
import Path
import Path.IO
import Test.Hspec
import System.Environment
import System.PosixCompat.Files

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

main :: IO ()
main = hspec . around withSandbox $ do
#ifndef mingw32_HOST_OS
  beforeWith populatedDir $ do
    -- NOTE These tests shall fail on Windows as unix-compat does not
    -- implement createSymbolicLink for windows.
    describe "listDir"          listDirSpec
    describe "listDirRecur"     listDirRecurSpec
    describe "listDirRecurWith" listDirRecurWithSpec
    describe "walkDir Finish"   walkDirFinishSpec
    describe "copyDirRecur"     copyDirRecurSpec
    describe "copyDirRecur'"    copyDirRecur'Spec
    describe "findFile"         findFileSpec
  beforeWith populatedCyclicDir $
    describe "listDirRecur Cyclic" listDirRecurCyclicSpec
#endif
  describe "getCurrentDir"    getCurrentDirSpec
  describe "setCurrentDir"    setCurrentDirSpec
  describe "withCurrentDir"   withCurrentDirSpec
  describe "walkDirRel"       walkDirRelSpec
#ifndef mingw32_HOST_OS
  -- NOTE We can't quite test this on Windows as well, because the
  -- environmental variables HOME and TMPDIR do not exist there.
  describe "getHomeDir"       getHomeDirSpec
  describe "getTempDir"       getTempDirSpec
#if MIN_VERSION_directory(1,2,3)
  describe "getXdgDir Data"   getXdgDataDirSpec
  describe "getXdgDir Config" getXdgConfigDirSpec
  describe "getXdgDir Cache"  getXdgCacheDirSpec
#endif
#endif

listDirSpec :: SpecWith (Path Abs Dir)
listDirSpec = it "lists directory" $ \dir ->
  getDirStructure listDir dir `shouldReturn` populatedDirTop

listDirRecurSpec :: SpecWith (Path Abs Dir)
listDirRecurSpec = it "lists directory recursively" $ \dir ->
  getDirStructure listDirRecur dir `shouldReturn` populatedDirStructure

listDirRecurWithSpec :: SpecWith (Path Abs Dir)
listDirRecurWithSpec =
  it "lists directory recursively using predicates" $ \dir ->
      getDirStructure (listDirRecurWith
                        (return . ($(mkRelDir "c") /=) . dirname)
                        (return . ($(mkRelFile "two.txt") /=) . filename)) dir
        `shouldReturn` populatedDirRecurWith

listDirRecurWith
  :: (Path Abs Dir ->  IO Bool)           -- ^ Dir match predicate
  -> (Path Abs File -> IO Bool)           -- ^ File match predicate
  -> Path Abs Dir                         -- ^ Top dir to traverse
  -> IO ([Path Abs Dir], [Path Abs File]) -- ^ Matched subdirs and files
listDirRecurWith dirPred filePred =
  walkDirAccum Nothing $ \_ d f -> do
    d' <- filterM dirPred  d
    f' <- filterM filePred f
    return (d', f')

-- Follows symbolic links
listDirRecurCyclic :: (MonadIO m, MonadThrow m)
  => Path b Dir                          -- ^ Directory to list
  -> m ([Path Abs Dir], [Path Abs File]) -- ^ Sub-directories and files
listDirRecurCyclic = walkDirAccum Nothing (\_ d f -> return (d, f))

listDirRecurCyclicSpec :: SpecWith (Path Abs Dir)
listDirRecurCyclicSpec =
  it "lists directory trees having traversal cycles" $ \dir ->
    getDirStructure listDirRecurCyclic dir
        `shouldReturn` populatedCyclicDirStructure

-- | walkDir with a Finish handler may have unpredictable output depending on
-- the order of traversal. The only guarantee is that we will finish only after
-- we find the directory "c". Though if we test only for the presence of "c" we
-- are not really testing if we indeed cut the traversal short.

walkDirFinishSpec :: SpecWith (Path Abs Dir)
walkDirFinishSpec =
  it "Finishes only after finding what it is looking for" $ \dir -> do
    (d, _) <- getDirStructure (walkDirAccum (Just dHandler) writer) dir
    map dirname d `shouldContain` [$(mkRelDir "c")]
    where dHandler p _ _
            | dirname p == $(mkRelDir "c") = return WalkFinish
            | otherwise = return (WalkExclude [])

          writer _ d f = return (d, f)

copyDirRecurSpec :: SpecWith (Path Abs Dir)
copyDirRecurSpec = do
  context "when source directory is editable" $
    it "copies directory" $ \src -> do
    let dest = parent src </> $(mkRelDir "copied-dir")
    copyDirRecur src dest
    old <- getDirStructure listDirRecur src
    new <- getDirStructure listDirRecur dest
    old `shouldBe` new
  context "when source directory is read-only" $
    it "copies directory just as well (preserving permissions)" $ \src -> do
    let dest = parent src </> $(mkRelDir "copied-dir")
    srcPermissions <- setOwnerWritable False <$> getPermissions src
    setPermissions src srcPermissions
    copyDirRecur src dest
    old <- getDirStructure listDirRecur src
    new <- getDirStructure listDirRecur dest
    old `shouldBe` new
    getPermissions dest `shouldReturn` srcPermissions

copyDirRecur'Spec :: SpecWith (Path Abs Dir)
copyDirRecur'Spec =
  context "when source directory is read-only" $
    it "copies directory but now it's editable" $ \src -> do
    let dest = parent src </> $(mkRelDir "copied-dir")
    srcPermissions <- setOwnerWritable False <$> getPermissions src
    setPermissions src srcPermissions
    copyDirRecur' src dest
    old <- getDirStructure listDirRecur src
    new <- getDirStructure listDirRecur dest
    old `shouldBe` new
    getPermissions dest `shouldReturn` srcPermissions { writable = True }

findFileSpec :: SpecWith (Path Abs Dir)
findFileSpec = it "finds a file lazily" $ \dir -> do
  let relFile = head (snd populatedDirTop)
  found <- findFile (dir : undefined) relFile
  found `shouldBe` Just (dir </> relFile)

getCurrentDirSpec :: SpecWith (Path Abs Dir)
getCurrentDirSpec = it "returns current dir" $ \dir ->
  getCurrentDir `shouldNotReturn` dir

setCurrentDirSpec :: SpecWith (Path Abs Dir)
setCurrentDirSpec = it "sets current dir" $ \dir -> do
  wdir <- getCurrentDir
  setCurrentDir dir
  new  <- getCurrentDir
  setCurrentDir wdir
  new `shouldBe` dir

withCurrentDirSpec :: SpecWith (Path Abs Dir)
withCurrentDirSpec = it "temporarily modifies current dir" $ \dir -> do
  withCurrentDir dir $
    getCurrentDir `shouldReturn` dir
  getCurrentDir `shouldNotReturn` dir

walkDirRelSpec :: SpecWith (Path Abs Dir)
walkDirRelSpec = it "does not throw exceptions" $ \dir -> do
  let handler curdir subdirs files = do
        curdir `shouldBe` $(mkRelDir ".")
        subdirs `shouldBe` []
        files `shouldBe` []
        return WalkFinish
  walkDirRel handler dir

getHomeDirSpec :: SpecWith (Path Abs Dir)
getHomeDirSpec =
  it "home dir is influenced by environment variable HOME" $ \dir ->
    bracket (getEnv evar) (setEnv evar) $ \_ -> do
      setEnv evar (toFilePath dir)
      getHomeDir `shouldReturn` dir
  where evar = "HOME"

getTempDirSpec :: SpecWith (Path Abs Dir)
getTempDirSpec =
  it "temp dir is influenced by environment variable TMPDIR" $ \dir ->
    flip finally (unsetEnv evar) $  do
      setEnv evar (toFilePath dir)
      getTempDir `shouldReturn` dir
      unsetEnv evar
  where evar = "TMPDIR"

#if MIN_VERSION_directory(1,2,3)
getXdgDataDirSpec :: SpecWith (Path Abs Dir)
getXdgDataDirSpec =
  it "XDG data dir is influenced by environment variable XDG_DATA_HOME" $ \dir ->
    flip finally (unsetEnv evar) $ do
      setEnv evar (toFilePath dir)
      getXdgDir XdgData (Just name) `shouldReturn` (dir </> name)
      getXdgDir XdgData Nothing `shouldReturn` dir
      unsetEnv evar
  where evar = "XDG_DATA_HOME"
        name = $(mkRelDir "test")

getXdgConfigDirSpec :: SpecWith (Path Abs Dir)
getXdgConfigDirSpec =
  it "XDG config dir is influenced by environment variable XDG_CONFIG_HOME" $ \dir ->
    flip finally (unsetEnv evar) $ do
      setEnv evar (toFilePath dir)
      getXdgDir XdgConfig (Just name) `shouldReturn` (dir </> name)
      getXdgDir XdgConfig Nothing `shouldReturn` dir
      unsetEnv evar
  where evar = "XDG_CONFIG_HOME"
        name = $(mkRelDir "test")

getXdgCacheDirSpec :: SpecWith (Path Abs Dir)
getXdgCacheDirSpec =
  it "XDG cache dir is influenced by environment variable XDG_CACHE_HOME" $ \dir ->
    flip finally (unsetEnv evar) $ do
      setEnv evar (toFilePath dir)
      getXdgDir XdgCache (Just name) `shouldReturn` (dir </> name)
      getXdgDir XdgCache Nothing `shouldReturn` dir
      unsetEnv evar
  where evar = "XDG_CACHE_HOME"
        name = $(mkRelDir "test")
#endif

----------------------------------------------------------------------------
-- Helpers

-- | Create a sandbox directory to model some situation in it and run some
-- tests. Note that we're using new unique sandbox directory for each test
-- case to avoid contamination and it's unconditionally deleted after test
-- case finishes.

withSandbox :: ActionWith (Path Abs Dir) -> IO ()
withSandbox = withSystemTempDir "path-io-sandbox"

-- | Create directory and some sub-directories and files in it. Return path
-- to that directory.
--
-- Created objects are described in 'populatedDirStructure'.

populatedDir :: Path Abs Dir -> IO (Path Abs Dir)
populatedDir root = do
  let (_, files) = populatedDirStructure
      pdir          = root </> $(mkRelDir "pdir")
      withinSandbox = (pdir </>)
  ensureDir pdir
  ensureDir $ withinSandbox $(mkRelDir "b")
  ensureDir $ withinSandbox $(mkRelDir "b/c")
  -- to verify that we do not follow symbolic links. We should not list b's
  -- tree under 'a'.
  createSymbolicLink "b" (toFilePath $ withinSandbox $(mkRelFile "a"))
  forM_ files $ (`writeFile` "") . toFilePath . withinSandbox
  return pdir

-- | Get inner structure of a directory. Items are sorted, so it's easier to
-- compare results.

getDirStructure
  :: (Path Abs Dir -> IO ([Path Abs Dir], [Path Abs File]))
     -- ^ Which function to use for scanning
  -> Path Abs Dir
     -- ^ Path to directory to scan
  -> IO ([Path Rel Dir], [Path Rel File])
getDirStructure f path = do
  (dirs, files) <- f path
  rdirs  <- sort <$> mapM (makeRelative path) dirs
  rfiles <- sort <$> mapM (makeRelative path) files
  return (rdirs, rfiles)

-- | Structure of directory created by the 'populatedDir' function. Please
-- keep it sorted.

populatedDirStructure :: ([Path Rel Dir], [Path Rel File])
populatedDirStructure =
  ( [ $(mkRelDir "a")
    , $(mkRelDir "b")
    , $(mkRelDir "b/c")
    ]
  , [ $(mkRelFile "b/c/three.txt")
    , $(mkRelFile "b/two.txt")
    , $(mkRelFile "one.txt")
    ]
  )

-- | Create a directory structure which has cycles in it due to directory
-- symbolic links.
--
-- 1) Mutual cycles between two directory trees. If we traverse @a@ or @c@ we
-- will get into the same cycle:
--     a\/(b -> c), c\/(d -> a)
--     c\/(d -> a), a\/(b -> c)
-- 2) Cycle with own ancestor
--     e\/f\/(g -> e)

populatedCyclicDirStructure :: ([Path Rel Dir], [Path Rel File])
populatedCyclicDirStructure =
  ( [
      $(mkRelDir "a")
    , $(mkRelDir "a/b")   -- b points to c
    , $(mkRelDir "a/b/d") -- because b is same as c
    , $(mkRelDir "c")
    , $(mkRelDir "c/d")   -- d points to a
    , $(mkRelDir "c/d/b") -- because d is same as a
    , $(mkRelDir "e")
    , $(mkRelDir "e/f")
    , $(mkRelDir "e/f/g") -- g points to e
    ]
  , []
  )

-- | Created the objects described in 'populatedCyclicDirStructure'.
-- Return path to that directory.

populatedCyclicDir :: Path Abs Dir -> IO (Path Abs Dir)
populatedCyclicDir root = do
  let pdir          = root </> $(mkRelDir "pdir")
      withinSandbox = (pdir </>)
  ensureDir pdir
  ensureDir $ withinSandbox $(mkRelDir "a")
  ensureDir $ withinSandbox $(mkRelDir "c")
  ensureDir $ withinSandbox $(mkRelDir "e/f")
  createSymbolicLink "../c" (toFilePath $ withinSandbox $(mkRelFile "a/b"))
  createSymbolicLink "../a" (toFilePath $ withinSandbox $(mkRelFile "c/d"))
  createSymbolicLink "../../e" (toFilePath $ withinSandbox $(mkRelFile "e/f/g"))
  return pdir

-- | Top-level structure of populated directory as it should be scanned by
-- the 'listDir' function.

populatedDirTop :: ([Path Rel Dir], [Path Rel File])
populatedDirTop =
  ( [ $(mkRelDir "a")
    , $(mkRelDir "b")
    ]
  , [ $(mkRelFile "one.txt")
    ]
  )

-- | Structure of populated directory as it should be scanned by
-- 'listDirRecurWith' function using predicates to filter out dir 'c' and the
-- file 'two.txt'

populatedDirRecurWith :: ([Path Rel Dir], [Path Rel File])
populatedDirRecurWith =
  ( [ $(mkRelDir "a")
    , $(mkRelDir "b")
    ]
  , [ $(mkRelFile "a/c/three.txt") -- via symbolic link
    , $(mkRelFile "b/c/three.txt")
    , $(mkRelFile "one.txt")
    ]
  )
