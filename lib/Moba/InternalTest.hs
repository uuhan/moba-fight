{-# OPTIONS_GHC -Wno-all #-}
{-# LANGUAGE QuasiQuotes #-}
module Moba.InternalTest
  ( tests
  ) where

import           Control.Monad                  (forM_)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as BL
import           Moba.Internal
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert)
import           Text.InterpolatedString.Perl6  (q)

tests :: Test
tests = testGroup "Client.Moba.Internal.Tests"
  [ testCase "FromServers" testUpdates
  , testCase "Ping" (test ping)
  , testCase "Picking" (test picking)
  , testCase "CmdError" (test cmderr)
  ]

testUpdates :: Assertion
testUpdates = do
    updates <- pure "./moba.log" >>= BL.readFile >>= pure . BL.lines
    forM_ (filter (not . BL.null) updates) $ \u -> test u

test input =
  assert $ case (eitherDecode input :: Either String FromServer) of
             Left s  -> error s
             Right _ -> True

ping = [q|{"type": "ping"}|]
picking = [q|{"type": "picking"}|]
cmderr = [q|{"type":"cmdError","cmd":{"type":"pickHero","heros":["warrior","warrior","shooter","shooter","shooter"]},"message":"message"}|]
