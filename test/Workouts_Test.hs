module Workouts_Test where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

workoutsSuite :: Test
workoutsSuite =
  testGroup "Workouts"
  [ testCase "Something" (testEmpty) ]

testEmpty :: Assertion
testEmpty = assertEqual 
  "a equals a"
  "aaa"
  "aaa"
