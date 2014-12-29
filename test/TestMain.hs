module Main where

import Test.Framework (defaultMain)

import Workouts_Test

main :: IO ()
main = defaultMain [workoutsSuite]
