module Workouts_Test where

import qualified Workouts as W

import Data.Time.Calendar (fromGregorian)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

workoutsSuite :: Test
workoutsSuite =
  testGroup "Workouts"
  [ testCase "Compute rest" testComputeRest ]

testComputeRest :: Assertion
testComputeRest =
  assertEqual "rest" [0, 1, 1, 10]
  (W.computeRest [ (fromGregorian 2000 1 1)
                 , (fromGregorian 2000 1 2)
                 , (fromGregorian 2000 1 3)
                 , (fromGregorian 2000 1 13)
                 ])
