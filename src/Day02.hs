module Day02 where

import Common.Runner
import Common.Parser
import Common.Util

part1 :: String -> Int
part1 = countIf safe . pInput

part2 :: String -> Int
part2 = countIf (\xs -> fixable xs || safe xs) . pInput
  where
    fixable xs = any (safe . snd) (removeOne xs)
    removeOne = go []
      where
        go _  [] = []
        go xs (y:ys) = (y, xs ++ ys) : go (xs ++ [y]) ys

safe :: [Int] -> Bool
safe xs = (allIncreasing || allDecreasing) && goodSeparations
  where
    ps = pairs xs
    allIncreasing = all (uncurry (<)) ps
    allDecreasing = all (uncurry (>)) ps
    goodSeparations = all (\(x, y) -> abs (x - y) `elem` [1..3]) ps



pInput :: String -> [[Int]]
pInput = pLines (pNumber `sepEndBy1` char ' ')

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 2

