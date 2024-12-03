module Day03 where

import Common.Runner
import Common.Parser
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)

part1 :: String -> Int
part1 i = sum [ x * y | Mul x y <- pInput i]

part2 :: String -> Int
part2 = snd . foldl' go (True, 0) . pInput
  where
    go (enabled, acc) = \case
      Do -> (True, acc)
      Dont -> (False, acc)
      Mul x y
        | enabled -> (True, acc + x * y)
        | otherwise -> (False, acc)

data Instr = Mul Int Int | Do | Dont deriving Show

pInput :: String -> [Instr]
pInput = catMaybes . pAll (many pOne)
  where
    pOne :: Parser (Maybe Instr)
    pOne = fmap Just (choice [ try pMul
                             , try (string "do()" $> Do)
                             , try (string "don't()" $> Dont)
                             ]) <|> (anySingle *> option Nothing pOne)
    pMul = do
      x <- string "mul(" *> pNumber
      y <- char ',' *> pNumber <* char ')'
      pure $ Mul x y

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 3

