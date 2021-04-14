module Test.Parser (parserTests) where

import Models

import Test.Helpers
import Test.HUnit

testBareMinimum :: Test
testBareMinimum = buildTestCaseForLine
  "Simple empty header"
  "* "
  (Left $ Header 1 Nothing Nothing "" Nothing)

testJustStatus :: Test
testJustStatus = buildTestCaseForLine
  "Simple header with keyword"
  "** DONE"
  (Left $ Header 2 (Just "DONE") Nothing "" Nothing)

testJustTitle :: Test
testJustTitle = buildTestCaseForLine
  "Simple header with no keywords"
  "*** Some e-mail"
  (Left $ Header 3 Nothing Nothing "Some e-mail" Nothing)

testAllFields :: Test
testAllFields = buildTestCaseForLine
  "Header with all fields"
  "**** TODO [#A] COMMENT Title :tag:a2%:"
  (Left $ Header 4 (Just "TODO") (Just 'A') "COMMENT Title" (Just ["tag", "a2%"]))

testJustTags :: Test
testJustTags = buildTestCaseForLine
  "Header with tag"
  "** :tag:a2%:"
  (Left $ Header 2 Nothing Nothing "" (Just ["tag", "a2%"]))

testHeaderLink :: Test
testHeaderLink = buildTestCaseForLine
  "Header with link"
  "* [[Hello world!]]"
  (Left $ Header 1 Nothing Nothing "[[Hello world!]]" Nothing)

testSimpleLine :: Test
testSimpleLine = buildTestCaseForLine
  "Simple non-header line"
  "Simple line"
  (Right $ Line [] "Simple line")

testSimpleLineWithLink :: Test
testSimpleLineWithLink = buildTestCaseForLine
  "Simple line with link"
  "Simple line with [[link]]"
  (Right $ Line ["link"] "Simple line with ")

testLineWith2Links :: Test
testLineWith2Links = buildTestCaseForLine
  "Line with 2 links"
  "Line with [[link]] and [[another]]"
  (Right $ Line ["link", "another"] "Line with  and ")

testLineWithMalformedLink :: Test
testLineWithMalformedLink = buildTestCaseForLine
  "Line with malformed link"
  "Line with [[malformed link]"
  (Right $ Line [] "Line with [[malformed link]")

parserTests :: [Test]
parserTests = [testBareMinimum, testJustStatus, testJustTitle, testJustTags, testAllFields,
              testHeaderLink, testSimpleLine, testSimpleLineWithLink, testLineWith2Links,
              testLineWithMalformedLink]
