module Day04 where

import Common.Runner
import Common.Grid
import Control.Monad (guard)
import Data.Map (Map)
import Data.Map qualified as M

-- For each possible starting point, check all eight directions
part1 :: String -> Int
part1 i = length $ do
        p <- M.keys grid                       -- Current word start
        d <- fullNeighbors 0                   -- Direction to search in
        let xs = take 4 $ iterate (+d) p       -- Points to examine
        guard $ Just "XMAS" == getWord grid xs -- Check if this is a solution
  where
    grid = pInput i

-- For each possible center of an x, check all four rotations
part2 :: String -> Int
part2 i = length $ do
        p <- M.keys grid                             -- Current x center
        ds <- take 4 $ iterate (map perp) xshape     -- Create the 4 x shapes needed
        let xs = map (p+) ds                         -- Points to examine
        guard $ Just "AMMSS" == getWord grid xs      -- Check if this is a solution
  where
    -- The five points that make up an x
    xshape :: [Point]
    xshape = 0 : liftA2 V2 [-1,1] [-1,1]
    grid = pInput i

-- Find the letters at the given point
-- Fail if not all in the grid given
getWord :: Map Point Char -> [Point] -> Maybe String
getWord grid = traverse (`M.lookup` grid)

pInput :: String -> Map Point Char
pInput = asciiGridMap Just

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 4

