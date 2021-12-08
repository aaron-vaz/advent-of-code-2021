{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed (embedStringFile)
import Lib

main :: IO ()
main = do
  let dayOneFile = $(embedStringFile "resources/day1-input.txt")
  let dayOneFileLines = lines dayOneFile
  let inputs = [read line :: Int | line <- dayOneFileLines]
  print $ "Day 1: " ++ show (dayOneExercise inputs)
  print $ "Day 1, Part 2: " ++ show (dayOneExercisePartTwo inputs)

  -- Day 2
  let dayTwoFile = $(embedStringFile "resources/day2-input.txt")
  let dayTwoFileLines = lines dayTwoFile
  print $ "Day 2: " ++ show (dayTwoExercise dayTwoFileLines)
  print $ "Day 2, Part 2: " ++ show (dayTwoExercisePartTwo dayTwoFileLines 0 (0, 0, 0))

  -- Day 3
  let dayThreeFile = $(embedStringFile "resources/day3-input.txt")
  let dayThreeFileLines = lines dayThreeFile
  print $ "Day 3: " ++ show (dayThreeExercise dayThreeFileLines)