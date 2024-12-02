module Day02 where

import Common.Runner
import Common.Parser
import Common.Util

part1 :: String -> Int
part1 = countIf safe . pInput

part2 :: String -> Int
part2 = countIf (\xs -> fixable xs || safe xs) . pInput
  where
    fixable xs = any safe (removeOne xs)
    -- This is a modified version of `select` from Common.Util
    -- This version keeps the elements of the list in the same order
    removeOne = go []
      where
        go _  [] = []
        go xs (y:ys) = (xs ++ ys) : go (xs ++ [y]) ys

-- | Check if a line is safe
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

