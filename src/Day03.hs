module Day03 where

import Common.Runner
import Common.Parser
import Data.Foldable (foldl')

-- The pattern match in the list comprehsion throws out all dos and donts
part1 :: String -> Int
part1 i = sum [ x * y | Mul x y <- pInput i]

-- Examine each operation one by one, updating the state as needed
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
pInput =  pSomeInput (many pOne)
  where
    -- Skip past all the junk until you either find another operation
   --  or encounter the end of the file
    pOne :: Parser Instr
    pOne = choice [ try pMul
                  , try (string "do()" $> Do)
                  , try (string "don't()" $> Dont)
                  ]
           <|> try (anySingle *> pOne)
    pMul = do
      x <- string "mul(" *> pNumber
      y <- char ',' *> pNumber <* char ')'
      pure $ Mul x y

solve :: Show a => (String -> a) -> IO (Either AoCError a)
solve = runSolutionOnInput 3

