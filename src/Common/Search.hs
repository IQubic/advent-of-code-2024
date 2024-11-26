{-

These implementations provide a lazily-generated list of visited
states with the order defined by the search strategy.

-}

module Common.Search (
  -- * Depth-first search
  dfs, dfsN, dfsOn, dfsOnN,

  -- * Breadth-first search
  bfs, bfsN, bfsOn, bfsOnN,

  -- * A* search
  AStep(..),
  astar, astarN, astarOn, astarOnN

  ) where

import Common.PQueue qualified as PQ
import Common.Queue qualified as Q
import Data.Foldable (foldl')
import Data.Set qualified as S
import Data.IntSet qualified as IS

-- | Shortcut for @'dfsOn' 'id'@
dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs = dfsOn id
{-# INLINE dfs #-}

-- | Shortcut for @'dfsOnN' 'id'@
dfsN :: Ord a => (a -> [a]) -> [a] -> [a]
dfsN = dfsOnN id
{-# INLINE dfsN #-}

-- | Depth-first search.
--
-- Generates the list of unique visited states from a
-- given starting state. States are unique up to the
-- characterizing function.
dfsOn ::
  Ord r =>
  (a -> r)   {- ^ state characterization              -} ->
  (a -> [a]) {- ^ successors function                 -} ->
  a          {- ^ initial state                       -} ->
  [a]        {- ^ visited states in depth-first order -}
dfsOn rep next start = dfsOnN rep next [start]
{-# INLINE dfsOn #-}

-- | Depth-first search.
--
-- Generates the list of unique visited states from a
-- given starting state. States are unique up to the
-- characterizing function.
dfsOnN ::
  Ord r =>
  (a -> r)   {- ^ state characterization              -} ->
  (a -> [a]) {- ^ successors function                 -} ->
  [a]        {- ^ initial states                      -} ->
  [a]        {- ^ visited states in depth-first order -}
dfsOnN rep next = loop S.empty
  where
    loop !seen = \case
      [] -> []
      x:xs
        | S.member r seen ->     loop seen xs
        | otherwise       -> x : loop seen' (next x ++ xs)
        where
          r     = rep x
          seen' = S.insert r seen

-- | Shortcut for @'bfsOn' 'id'@
bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id
{-# INLINE bfs #-}

-- | Shortcut for @'bfsOnN' 'id'@
bfsN :: Ord a => (a -> [a]) -> [a] -> [a]
bfsN = bfsOnN id
{-# INLINE bfsN #-}

-- | Enumerate the reachable states in breadth-first order
-- given a successor state function and initial state.
--
-- States are compared for equality using the representative
-- function. If the representatives are equal the state is
-- considered already visited.
{-# INLINE [0] bfsOn #-}
bfsOn ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  a          {- ^ initial state             -} ->
  [a]        {- ^ reachable states          -}
bfsOn rep next start = bfsOnN rep next [start]

-- | Generalization of 'bfsOn' allowing multiple
-- initial states to be considered.
bfsOnN ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  [a]        {- ^ initial states            -} ->
  [a]        {- ^ reachable states          -}
bfsOnN rep next start = loop S.empty (Q.fromList start)
  where
    loop !seen = \case
      Q.Empty -> []
      x Q.:<| q
        | S.member r seen ->     loop seen  q
        | otherwise       -> x : loop seen' q'
        where
          r     = rep x
          seen' = S.insert r seen
          q'    = Q.appendList q (next x)
{-# INLINE [0] bfsOnN #-}

{-# RULES "bfsOn/Int" bfsOn = bfsOnInt #-}
{-# INLINE bfsOnInt #-}
bfsOnInt :: (a -> Int) -> (a -> [a]) -> a -> [a]
bfsOnInt rep next start = loop IS.empty (Q.singleton start)
  where
    loop !seen = \case
      Q.Empty -> []
      x Q.:<| q
        | IS.member r seen ->     loop seen  q
        | otherwise        -> x : loop seen' q'
        where
          r     = rep x
          seen' = IS.insert r seen
          q'    = Q.appendList q (next x)

-- | Shortcut for @'astarOn' 'id'@
astar :: Ord a => (a -> [AStep a]) -> a -> [(a,Int)]
astar = astarOn id
{-# INLINE astar #-}

-- | Shortcut for @'astarOnN' 'id'@
astarN :: Ord a => (a -> [AStep a]) -> [a] -> [(a,Int)]
astarN = astarOnN id
{-# INLINE astarN #-}

-- | A* graph search producing a list of reached states and the
-- minimum cost of reaching that state.
--
-- Returned states will be unique up to the characterization function.
-- This allows extra information of a node to be ignored for the
-- purposes of the search. For example, a node might remember the
-- path used to reach it while for the search the particular path
-- taken might not matter.
astarOn ::
  Ord b =>
  (a -> b)         {- ^ state characterization                                   -} ->
  (a -> [AStep a]) {- ^ step function (new state, step cost, distance heuristic) -} ->
  a                {- ^ starting state                                           -} ->
  [(a,Int)]        {- ^ list of states visited                                   -}
astarOn rep nexts start = astarOnN rep nexts [start]

-- | Generalization of 'astarOn' that accepts multiple starting states.
astarOnN ::
  Ord b =>
  (a -> b)         {- ^ state characterization                                   -} ->
  (a -> [AStep a]) {- ^ step function (new state, step cost, distance heuristic) -} ->
  [a]              {- ^ starting states                                          -} ->
  [(a,Int)]        {- ^ list of states visited                                   -}
astarOnN rep nexts starts = go S.empty (PQ.fromList [(0, WC 0 s) | s <- starts])
  where
    go !seen = \case
      PQ.Empty -> []
      WC cost x PQ.:<| work
        | S.member r seen -> go seen work
        | otherwise       -> (x,cost) : go seen' work'
        where
          r     = rep x
          seen' = S.insert r seen
          work' = foldl' addWork work (nexts x)
          addWork w (AStep x' stepcost heuristic) =
            PQ.insert (cost' + heuristic) (WC cost' x') w
            where
              cost' = cost + stepcost
{-# INLINE astarOn #-}

-- Helper type to unpack the cost value in the A* priority queue
data WithCost a = WC !Int a

-- | A step in the A* graph search annotated with its cost and an
-- estimate of the distance remaining to the goal. The estimate
-- must be an underapproximation to ensure the search finds the
-- optimal solution
data AStep a = AStep {
  astepNext      :: a,    -- ^ successor node
  astepCost      :: !Int, -- ^ cost of edge
  astepHeuristic :: !Int  -- ^ heuristic cost to goal from this new node
  } deriving Show
