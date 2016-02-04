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

module Main (main) where

import Path
import Path.IO
import Test.Hspec

main :: IO ()
main = hspec . around withSandbox $ do
  describe "listDir"        listDirSpec
  describe "listDirRecur"   listDirRecurSpec
  describe "copyDirRecur"   copyDirRecurSpec
  describe "getCurrentDir"  getCurrentDirSpec
  describe "setCurrentDir"  setCurrentDirSpec
  describe "withCurrentDir" withCurrentDirSpec
  describe "getHomeDir"     getHomeDirSpec
  describe "getTempDir"     getTempDirSpec

listDirSpec :: SpecWith (Path Abs Dir)
listDirSpec = it "FIXME" (const pending)

listDirRecurSpec :: SpecWith (Path Abs Dir)
listDirRecurSpec = it "FIXME" (const pending)

copyDirRecurSpec :: SpecWith (Path Abs Dir)
copyDirRecurSpec = it "FIXME" (const pending)

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
withSandbox = withSystemTempDir "plan-b-sandbox"
