module Main (main) where

import Test.Parser
import Test.Helpers
import qualified Test.Framework

allTests :: [Test.Framework.Test]
allTests = convertAllTests parserTests

main :: IO ()
main = Test.Framework.defaultMainWithOpts allTests mempty
