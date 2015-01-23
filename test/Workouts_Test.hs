module Workouts_Test where

import qualified Workouts as W

import Data.Time.Calendar (fromGregorian)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

workoutsSuite :: Test
workoutsSuite =
  testGroup "Workouts"
  [ testCase "" $ testComputeRest
  , testCase "" $ testRankAsc
  , testCase "" $ testParseDuration "20:00" (Just $ 20 * 60)
  , testCase "" $ testParseDuration "1:00:00" (Just $ 60 * 60)
  , testCase "" $ testParseDuration "foo" Nothing
  ]

testComputeRest :: Assertion
testComputeRest =
  assertEqual "rest" [0, 0, 0, 9, 0] $
  W.computeRest [ (fromGregorian 2000 1 1)
                , (fromGregorian 2000 1 2)
                , (fromGregorian 2000 1 3)
                , (fromGregorian 2000 1 13)
                , (fromGregorian 2000 1 13)
                ]

testRankAsc :: Assertion
testRankAsc =
  assertEqual "rankAsc" [3, 0, 2, 1] $
  W.rankAsc [10, 1, 9, 2]

testParseDuration :: String -> Maybe Int -> Assertion
testParseDuration input output =
  assertEqual ("parseDuration: " ++ input) output (W.parseDuration input)
