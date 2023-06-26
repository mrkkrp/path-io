{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor
import Data.List (sort)
import Path
import Path.IO
import System.Environment
import System.PosixCompat.Files
import Test.Hspec

main :: IO ()
main = hspec . around withSandbox $ do

{- ORMOLU_DISABLE -}

#ifndef mingw32_HOST_OS
  beforeWith populatedDir $ do
    -- NOTE These tests fail on Windows as unix-compat does not implement
    -- createSymbolicLink for windows.
    describe "listDir"          listDirSpec
    describe "listDirRel"       listDirRelSpec
    describe "listDirRecur"     listDirRecurSpec
    describe "listDirRecurRel"  listDirRecurRelSpec
    describe "listDirRecurWith" listDirRecurWithSpec
    describe "walkDir Finish"   walkDirFinishSpec
    describe "copyDirRecur"     copyDirRecurSpec
    describe "copyDirRecur'"    copyDirRecur'Spec
    describe "findFile"         findFileSpec
    describe "removeDirLink"    removeDirLinkSpec
  beforeWith populatedCyclicDir $
    describe "listDirRecur Cyclic" listDirRecurCyclicSpec
#endif
  describe "getCurrentDir" getCurrentDirSpec
  describe "setCurrentDir" setCurrentDirSpec
  describe "withCurrentDir" withCurrentDirSpec
  describe "walkDirRel" walkDirRelSpec
#ifndef mingw32_HOST_OS
  -- NOTE We can't quite test this on Windows as well, because the
  -- environmental variables HOME and TMPDIR do not exist there.
  describe "getHomeDir"       getHomeDirSpec
  describe "getTempDir"       getTempDirSpec
  describe "getXdgDir Data"   getXdgDataDirSpec
  describe "getXdgDir Config" getXdgConfigDirSpec
  describe "getXdgDir Cache"  getXdgCacheDirSpec
#endif

{- ORMOLU_ENABLE -}

listDirSpec :: SpecWith (Path Abs Dir)
listDirSpec = it "lists directory" $ \dir ->
  getDirStructure listDir dir `shouldReturn` populatedDirTop

listDirRelSpec :: SpecWith (Path Abs Dir)
listDirRelSpec = it "lists directory" $ \dir ->
  getDirStructureRel listDirRel dir `shouldReturn` populatedDirTop

listDirRecurSpec :: SpecWith (Path Abs Dir)
listDirRecurSpec = it "lists directory recursively" $ \dir ->
  getDirStructure listDirRecur dir `shouldReturn` populatedDirStructure

listDirRecurRelSpec :: SpecWith (Path Abs Dir)
listDirRecurRelSpec = it "lists directory recursively" $ \dir ->
  getDirStructureRel listDirRecurRel dir `shouldReturn` populatedDirStructure

listDirRecurWithSpec :: SpecWith (Path Abs Dir)
listDirRecurWithSpec =
  it "lists directory recursively using predicates" $ \dir ->
    getDirStructure
      ( listDirRecurWith
          (return . ($(mkRelDir "c") /=) . dirname)
          (return . ($(mkRelFile "two.txt") /=) . filename)
      )
      dir
      `shouldReturn` populatedDirRecurWith

listDirRecurWith ::
  -- | Dir match predicate
  (Path Abs Dir -> IO Bool) ->
  -- | File match predicate
  (Path Abs File -> IO Bool) ->
  -- | Top dir to traverse
  Path Abs Dir ->
  -- | Matched subdirs and files
  IO ([Path Abs Dir], [Path Abs File])
listDirRecurWith dirPred filePred =
  walkDirAccum Nothing $ \_ d f -> do
    d' <- filterM dirPred d
    f' <- filterM filePred f
    return (d', f')

-- | 'walkDir' with a 'WalkFinish' handler may have unpredictable output
-- depending on the order of traversal. The only guarantee is that we will
-- finish only after we find the directory "c". Though if we test only for
-- the presence of "c" we are not really testing if we indeed cut the
-- traversal short.
walkDirFinishSpec :: SpecWith (Path Abs Dir)
walkDirFinishSpec =
  it "Finishes only after finding what it is looking for" $ \dir -> do
    (d, _) <- getDirStructure (walkDirAccum (Just dHandler) writer) dir
    map dirname d `shouldContain` [$(mkRelDir "c")]
  where
    dHandler p _ _
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
      getPermissions dest `shouldReturn` srcPermissions {writable = True}

findFileSpec :: SpecWith (Path Abs Dir)
findFileSpec = it "finds a file lazily" $ \dir -> do
  let relFile = head (snd populatedDirTop)
  found <- findFile (dir : undefined) relFile
  found `shouldBe` Just (dir </> relFile)

removeDirLinkSpec :: SpecWith (Path Abs Dir)
removeDirLinkSpec = it "remove dir link" $ \dir -> do
  let target = dir </> $(mkRelDir "a")
      link = dir </> $(mkRelDir "link-a")
  createDirLink target link
  removeDirLink link
  exists <- doesDirExist link
  exists `shouldBe` False

listDirRecurCyclicSpec :: SpecWith (Path Abs Dir)
listDirRecurCyclicSpec =
  it "lists directory trees having traversal cycles" $ \dir ->
    getDirStructure listDirRecurCyclic dir
      `shouldReturn` populatedCyclicDirStructure

-- | Follows symbolic links.
listDirRecurCyclic ::
  (MonadIO m) =>
  -- | Directory to list
  Path b Dir ->
  -- | Sub-directories and files
  m ([Path Abs Dir], [Path Abs File])
listDirRecurCyclic = walkDirAccum Nothing (\_ d f -> return (d, f))

getCurrentDirSpec :: SpecWith (Path Abs Dir)
getCurrentDirSpec = it "returns current dir" $ \dir ->
  getCurrentDir `shouldNotReturn` dir

setCurrentDirSpec :: SpecWith (Path Abs Dir)
setCurrentDirSpec = it "sets current dir" $ \dir -> do
  wdir <- getCurrentDir
  setCurrentDir dir
  new <- getCurrentDir
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
  where
    evar = "HOME"

getTempDirSpec :: SpecWith (Path Abs Dir)
getTempDirSpec =
  it "temp dir is influenced by environment variable TMPDIR" $ \dir ->
    flip finally (unsetEnv evar) $ do
      setEnv evar (toFilePath dir)
      getTempDir `shouldReturn` dir
      unsetEnv evar
  where
    evar = "TMPDIR"

getXdgDataDirSpec :: SpecWith (Path Abs Dir)
getXdgDataDirSpec =
  it "XDG data dir is influenced by environment variable XDG_DATA_HOME" $ \dir ->
    flip finally (unsetEnv evar) $ do
      setEnv evar (toFilePath dir)
      getXdgDir XdgData (Just name) `shouldReturn` (dir </> name)
      getXdgDir XdgData Nothing `shouldReturn` dir
      unsetEnv evar
  where
    evar = "XDG_DATA_HOME"
    name = $(mkRelDir "test")

getXdgConfigDirSpec :: SpecWith (Path Abs Dir)
getXdgConfigDirSpec =
  it "XDG config dir is influenced by environment variable XDG_CONFIG_HOME" $ \dir ->
    flip finally (unsetEnv evar) $ do
      setEnv evar (toFilePath dir)
      getXdgDir XdgConfig (Just name) `shouldReturn` (dir </> name)
      getXdgDir XdgConfig Nothing `shouldReturn` dir
      unsetEnv evar
  where
    evar = "XDG_CONFIG_HOME"
    name = $(mkRelDir "test")

getXdgCacheDirSpec :: SpecWith (Path Abs Dir)
getXdgCacheDirSpec =
  it "XDG cache dir is influenced by environment variable XDG_CACHE_HOME" $ \dir ->
    flip finally (unsetEnv evar) $ do
      setEnv evar (toFilePath dir)
      getXdgDir XdgCache (Just name) `shouldReturn` (dir </> name)
      getXdgDir XdgCache Nothing `shouldReturn` dir
      unsetEnv evar
  where
    evar = "XDG_CACHE_HOME"
    name = $(mkRelDir "test")

----------------------------------------------------------------------------
-- Helpers

-- | Create a sandbox directory to model some situation in it and run some
-- tests. Note that we're using a new unique sandbox directory for each test
-- case and it's unconditionally deleted after test case finishes.
withSandbox :: ActionWith (Path Abs Dir) -> IO ()
withSandbox = withSystemTempDir "path-io-sandbox"

-- | Create directory and some sub-directories and files in it. Return path
-- to that directory.
--
-- Created objects are described in 'populatedDirStructure'.
populatedDir :: Path Abs Dir -> IO (Path Abs Dir)
populatedDir root = do
  let (_, files) = populatedDirStructure
      pdir = root </> $(mkRelDir "pdir")
      withinSandbox = (pdir </>)
  ensureDir pdir
  let b = withinSandbox $(mkRelDir "b")
  ensureDir b
  ensureDir $ withinSandbox $(mkRelDir "b/c")
  -- Create a read-only directory with a file inside to test that the code
  -- that copies directory permissions can handle that gracefully.
  --
  -- See: https://github.com/mrkkrp/path-io/pull/82
  let readonlyDir = withinSandbox $(mkRelDir "readonly-dir")
  ensureDir readonlyDir
  -- We should not list b's tree under 'a' in order to verify that we do not
  -- follow symbolic links.
  createSymbolicLink "b" (toFilePath $ withinSandbox $(mkRelFile "a"))
  forM_ files $ (`writeFile` "") . toFilePath . withinSandbox
  getPermissions readonlyDir
    >>= setPermissions readonlyDir . setOwnerWritable False
  return pdir

-- | Get the inner structure of a directory. Items are sorted, so it's
-- easier to compare results.
getDirStructure ::
  -- | Which function to use for scanning
  (Path Abs Dir -> IO ([Path Abs Dir], [Path Abs File])) ->
  -- | Path to directory to scan
  Path Abs Dir ->
  IO ([Path Rel Dir], [Path Rel File])
getDirStructure f path = do
  (dirs, files) <- f path
  rdirs <- sort <$> mapM (makeRelative path) dirs
  rfiles <- sort <$> mapM (makeRelative path) files
  return (rdirs, rfiles)

-- | A version of 'getDirStructure' that accepts scanning function that
-- returns relative paths.
getDirStructureRel ::
  -- | Which function to use for scanning
  (Path Abs Dir -> IO ([Path Rel Dir], [Path Rel File])) ->
  -- | Path to directory to scan
  Path Abs Dir ->
  IO ([Path Rel Dir], [Path Rel File])
getDirStructureRel f path = bimap sort sort <$> f path

-- | Structure of directory created by the 'populatedDir' function. Please
-- keep it sorted.
populatedDirStructure :: ([Path Rel Dir], [Path Rel File])
populatedDirStructure =
  ( [ $(mkRelDir "a"),
      $(mkRelDir "b"),
      $(mkRelDir "b/c"),
      $(mkRelDir "readonly-dir")
    ],
    [ $(mkRelFile "b/c/three.txt"),
      $(mkRelFile "b/two.txt"),
      $(mkRelFile "one.txt"),
      $(mkRelFile "readonly-dir/two.txt")
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
  ( [ $(mkRelDir "a"),
      $(mkRelDir "a/b"), -- b points to c
      $(mkRelDir "a/b/d"), -- because b is same as c
      $(mkRelDir "c"),
      $(mkRelDir "c/d"), -- d points to a
      $(mkRelDir "c/d/b"), -- because d is same as a
      $(mkRelDir "e"),
      $(mkRelDir "e/f"),
      $(mkRelDir "e/f/g") -- g points to e
    ],
    []
  )

-- | Created the objects described in 'populatedCyclicDirStructure'.
-- Return path to that directory.
populatedCyclicDir :: Path Abs Dir -> IO (Path Abs Dir)
populatedCyclicDir root = do
  let pdir = root </> $(mkRelDir "pdir")
      withinSandbox = (pdir </>)
  ensureDir pdir
  ensureDir $ withinSandbox $(mkRelDir "a")
  ensureDir $ withinSandbox $(mkRelDir "c")
  ensureDir $ withinSandbox $(mkRelDir "e/f")
  createSymbolicLink "../c" (toFilePath $ withinSandbox $(mkRelFile "a/b"))
  createSymbolicLink "../a" (toFilePath $ withinSandbox $(mkRelFile "c/d"))
  createSymbolicLink "../../e" (toFilePath $ withinSandbox $(mkRelFile "e/f/g"))
  return pdir

-- | Top-level structure of the populated directory as it should be scanned
-- by the 'listDir' function.
populatedDirTop :: ([Path Rel Dir], [Path Rel File])
populatedDirTop =
  ( [ $(mkRelDir "a"),
      $(mkRelDir "b"),
      $(mkRelDir "readonly-dir")
    ],
    [ $(mkRelFile "one.txt")
    ]
  )

-- | Structure of the populated directory as it should be scanned by
-- 'listDirRecurWith' function using predicates to filter out dir 'c' and
-- the file @two.txt@.
populatedDirRecurWith :: ([Path Rel Dir], [Path Rel File])
populatedDirRecurWith =
  ( [ $(mkRelDir "a"),
      $(mkRelDir "b"),
      $(mkRelDir "readonly-dir")
    ],
    [ $(mkRelFile "a/c/three.txt"), -- via a symbolic link
      $(mkRelFile "b/c/three.txt"),
      $(mkRelFile "one.txt")
    ]
  )
