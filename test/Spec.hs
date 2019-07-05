{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Tasty.HUnit
import Test.Tasty

import Data.Text as T
import CodeBlockExecutor
import Control.Applicative


input1 :: [Text]
input1 = [">> putStrLn \"This string should show up in the output\""]
ghci_result1 :: [Text]
ghci_result1 = ["This string should show up in the output"]
combined_result1 :: String
combined_result1 = T.unpack . T.unlines $
  [
    ">> putStrLn \"This string should show up in the output\"",
    "This string should show up in the output"
  ]

input2 :: [Text]
input2 = fmap T.pack [
  "testFunc:: Integer -> Integer\ntestFunc x = x + 1",
  ">> testFunc 13\n",
  "anotherFunc:: Integer -> Integer\nanotherFunc x = x * 2"
  ]

ghci_result2 :: [Text]
ghci_result2 = fmap T.pack ["", "14", ""]
combined_result2 :: String
combined_result2  = T.unpack . T.unlines $
  ["testFunc:: Integer -> Integer",
  "testFunc x = x + 1",
  "",
  ">> testFunc 13",
  "14",
  "anotherFunc:: Integer -> Integer",
  "anotherFunc x = x * 2",
  ""]

fixtures :: [([Text], [Text], String)]
fixtures = [
  (input1, ghci_result1, combined_result1),
  (input2, ghci_result2, combined_result2)]

generateTests :: [TestTree]
generateTests = getZipList $ testProcessResults <$> ZipList fixtures <*> ZipList [1..]

testProcessResults :: Show a => ([Text], [Text], String) -> a -> TestTree
testProcessResults (input, ghci_results, expected) i = testCase ("test_processResults" ++ (show i)) $ do
  let actual = processResults input ghci_results
  assertEqual "processResult" expected actual

-- test_processResults = testCase "test_processResults" (
--   assertEqual "input1" combined_result (processResults [input1] [ghci_result]))

-- test_processResults2 = testCase "test_processResults" (
--   assertEqual "input1" combined_result (processResults [input1] [ghci_result]))

tests:: TestTree
tests = testGroup "Tests" generateTests

main :: IO ()
main = defaultMain tests

--runTestTT $ TestList [TestLabel "test1" test_processResults]
