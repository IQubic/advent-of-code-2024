module Day25 where

import Common.Runner
import Common.Parser
import Common.Util

-- part1 :: String -> Int
part1 = undefined

-- pInput :: String -> [a]
pInput = pLines $ undefined

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 25

