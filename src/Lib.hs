module Lib (dayOneExercise, dayOneExercisePartTwo, dayTwoExercise, dayTwoExercisePartTwo, dayThreeExercise) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt)
import Data.List (findIndices, foldl')
import Debug.Trace

indexedList :: [b] -> [(Int, b)]
indexedList = zip [0 ..]

binToDec :: String -> Int
binToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- Day 1

hasMeasurementIncreased :: [Int] -> Int -> Bool
hasMeasurementIncreased _ 0 = False
hasMeasurementIncreased inputs index = inputs !! index > inputs !! (index - 1)

dayOneExercise :: [Int] -> Int
dayOneExercise inputs = length [value | (index, value) <- indexedList inputs, hasMeasurementIncreased inputs index]

-- Day 1, Part 2

createThreeMeasurementSum :: [Int] -> Int -> Int
createThreeMeasurementSum inputs startIndex = sum $ take 3 $ drop startIndex inputs

dayOneExercisePartTwo :: [Int] -> Int
dayOneExercisePartTwo inputs = dayOneExercise [createThreeMeasurementSum inputs index | (index, _) <- indexedList inputs]

-- Day 2

calculateRoute :: String -> (Int, Int)
calculateRoute input
  | direction == "forward " = (distance, 0)
  | direction == "down " = (0, distance)
  | direction == "up " = (0, negate distance)
  | otherwise = (0, 0)
  where
    direction = init input
    distance = digitToInt $ last input

dayTwoExercise :: [String] -> Int
dayTwoExercise inputs = do
  let route = [calculateRoute input | input <- inputs]
  let result = foldl (\x acc -> bimap (fst acc +) (snd acc +) x) (0, 0) route
  uncurry (*) result

-- Day 2, Part 2

calculateRouteWithAim :: String -> (Int, Int, Int) -> (Int, Int, Int)
calculateRouteWithAim input (horizontal, depth, aim)
  | direction == "forward " = (horizontal + distance, depth + aim * distance, aim)
  | direction == "down " = (horizontal, depth, aim + distance)
  | direction == "up " = (horizontal, depth, aim - distance)
  | otherwise = (horizontal, depth, aim)
  where
    direction = init input
    distance = digitToInt $ last input

calculateFinalResult :: (Int, Int, Int) -> Int
calculateFinalResult (horizontal, depth, _) = horizontal * depth

dayTwoExercisePartTwo :: [String] -> Int -> (Int, Int, Int) -> Int
dayTwoExercisePartTwo inputs index initial
  | index >= inputsSize = calculateFinalResult initial
  | otherwise = do
    let result = calculateRouteWithAim (inputs !! index) initial
    dayTwoExercisePartTwo inputs (index + 1) result
  where
    inputsSize = length inputs

-- Day 3
countOccurrences :: Char -> String -> Int
countOccurrences character inputs = length $ filter (character ==) inputs

countOfBits :: [String] -> Int -> (Int, Int)
countOfBits inputs indexOfBit = do
  let bits = [input !! indexOfBit | input <- inputs]
  (countOccurrences '0' bits, countOccurrences '1' bits)

calculateMostSignificantBitAtIndex :: [String] -> Int -> Char
calculateMostSignificantBitAtIndex inputs indexOfBit = do
  let (occOfZero, occOfOne) = countOfBits inputs indexOfBit
  if occOfZero > occOfOne then '0' else '1'

calculateLeastSignificantBitAtIndex :: [String] -> Int -> Char
calculateLeastSignificantBitAtIndex inputs indexOfBit = do
  let (occOfZero, occOfOne) = countOfBits inputs indexOfBit
  if occOfZero < occOfOne then '0' else '1'

calculateGammaRate :: [String] -> Int -> String -> Int
calculateGammaRate inputs index initial
  | index >= inputsSize = binToDec initial
  | otherwise = do
    let significantBit = initial ++ [calculateMostSignificantBitAtIndex inputs index]
    calculateGammaRate inputs (index + 1) significantBit
  where
    inputsSize = length $ head inputs

calculateEpsilonRate :: [String] -> Int -> String -> Int
calculateEpsilonRate inputs index initial
  | index >= inputsSize = binToDec initial
  | otherwise = do
    let significantBit = initial ++ [calculateLeastSignificantBitAtIndex inputs index]
    calculateEpsilonRate inputs (index + 1) significantBit
  where
    inputsSize = length $ head inputs

dayThreeExercise :: [String] -> Int
dayThreeExercise inputs = calculateGammaRate inputs 0 "" * calculateEpsilonRate inputs 0 ""