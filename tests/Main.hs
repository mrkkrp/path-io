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
import Data.List (sort)
import Path
import Path.IO
import Test.Hspec

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

main :: IO ()
main = hspec . around withSandbox $ do
  beforeWith populatedDir $ do
    describe "listDir"      listDirSpec
    describe "listDirRecur" listDirRecurSpec
    describe "copyDirRecur" copyDirRecurSpec
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

getCurrentDirSpec :: SpecWith (Path Abs Dir)
getCurrentDirSpec = it "FIXME" (const pending)

setCurrentDirSpec :: SpecWith (Path Abs Dir)
setCurrentDirSpec = it "FIXME" (const pending)

withCurrentDirSpec :: SpecWith (Path Abs Dir)
withCurrentDirSpec = it "FIXME" (const pending)

getHomeDirSpec :: SpecWith (Path Abs Dir)
getHomeDirSpec = it "FIXME" (const pending)

getTempDirSpec :: SpecWith (Path Abs Dir)
getTempDirSpec = it "FIXME" (const pending)

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
