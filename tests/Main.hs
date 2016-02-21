--
-- Tests for ‘path-io’ package.
--
-- Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Control.Monad.Catch
import Data.List (sort)
import Path
import Path.IO
import Test.Hspec
import System.Environment

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

main :: IO ()
main = hspec . around withSandbox $ do
  beforeWith populatedDir $ do
    describe "listDir"      listDirSpec
    describe "listDirRecur" listDirRecurSpec
    describe "copyDirRecur" copyDirRecurSpec
    describe "findFile"     findFileSpec
  describe "getCurrentDir"  getCurrentDirSpec
  describe "setCurrentDir"  setCurrentDirSpec
  describe "withCurrentDir" withCurrentDirSpec
  describe "getHomeDir"     getHomeDirSpec
  describe "getTempDir"     getTempDirSpec

listDirSpec :: SpecWith (Path Abs Dir)
listDirSpec = it "lists directory" $ \dir ->
  getDirStructure listDir dir `shouldReturn` populatedDirTop

listDirRecurSpec :: SpecWith (Path Abs Dir)
listDirRecurSpec = it "lists directory recursively" $ \dir ->
  getDirStructure listDirRecur dir `shouldReturn` populatedDirStructure

copyDirRecurSpec :: SpecWith (Path Abs Dir)
copyDirRecurSpec = it "copies directory" $ \src -> do
  let dest = parent src </> $(mkRelDir "copied-dir")
  copyDirRecur src dest
  old <- getDirStructure listDirRecur src
  new <- getDirStructure listDirRecur dest
  old `shouldBe` new

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

----------------------------------------------------------------------------
-- Helpers

-- | Create sandbox directory to model some situation in it and run some
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
  let (dirs, files) = populatedDirStructure
      pdir          = root </> $(mkRelDir "pdir")
      withinSandbox = (pdir </>)
  ensureDir pdir
  forM_ dirs (ensureDir . withinSandbox)
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
