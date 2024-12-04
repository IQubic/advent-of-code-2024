module Day01 where

import Common.Runner
import Common.Parser
import Common.Util
import Data.List (sort)
import Data.IntMap qualified as IM

part1 :: String -> Int
part1 i = sum $ zipWith dist (sort ls) (sort rs)
  where
    dist l r = abs (l - r)
    (ls, rs) = pInput i

part2 :: String -> Int
part2 i = sum $ map (score (intFreqs rs)) ls
  where
    score rFreqs l = l * IM.findWithDefault 0 l rFreqs
    (ls, rs) = pInput i

pInput :: String -> ([Int], [Int])
pInput = unzip . pLines pPair
  where
    pPair = do
      l <- pNumber <* hspace
      r <- pNumber
      pure (l, r)

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 1
