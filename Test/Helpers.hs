module Test.Helpers where

import Models
import Parser

import Test.HUnit
import Test.Framework.Providers.HUnit
import qualified Test.Framework

buildTestCaseForLine :: String -> String -> OrgDocLine -> Test
buildTestCaseForLine description input expected = description ~: parseLine input @=? Right expected

convertAllTests :: [Test] -> [Test.Framework.Test]
convertAllTests tests = mconcat $ hUnitTestToTests <$> tests
