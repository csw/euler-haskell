import Data.Array.IArray
import Data.Array.Unboxed
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ


-- P107: Minimal network 
-- 
-- Given a 40-vertex network in matrix form, find the maximum saving
-- which can be achieved by removing redundant edges whilst ensuring
-- that the network remains connected.
--
-- Amounts to finding the MST, and the difference between its total
-- edge weights and that of the original network.

-- Graph data types

newtype Vertex = Vertex { vid :: Int }
    deriving (Eq, Show, Ord, Ix)

data AdjMatrix = AdjMatrix Int (Array Vertex (UArray Vertex Int))

data Edge = Edge Vertex Vertex
    deriving (Eq, Show, Ord)

degree :: AdjMatrix -> Int
degree (AdjMatrix deg _) = deg

vertices :: AdjMatrix -> [Vertex]
vertices mat = map Vertex [1..(degree mat)]

distance :: AdjMatrix -> Vertex -> Vertex -> Maybe Int
distance (AdjMatrix _ mat) u v =
    case (mat ! u) ! v of
      0 -> Nothing
      n -> Just n

neighbors :: AdjMatrix -> Vertex -> [(Vertex, Int)]
neighbors (AdjMatrix deg mat) u =
    filter conn $ assocs (mat ! u)
    where conn (v, dist) = dist /= 0

neighborsIn :: AdjMatrix -> Set Vertex -> Vertex -> [(Vertex, Int)]
neighborsIn mat outV u =
    mapMaybe (maybePair $ distance mat u) $ Set.elems outV

edgeWeight :: AdjMatrix -> Edge -> Int
edgeWeight (AdjMatrix _ mat) (Edge u v) = (mat ! u) ! v

totalWeight :: AdjMatrix -> Int
totalWeight (AdjMatrix deg mat) = sum . map vWeight $ assocs mat
    where vWeight (u, adj) = sum $ map ((adj !) . Vertex) [((vid u)+1)..deg]

-- Parse a graph, find the MST, and compute the savings
-- = 259679
p107 :: String -> Int
p107 str = totalWeight mat - mstWeight
    where mat = parseMatrix str
          (_, mstWeight) = prims mat

p107p :: (AdjMatrix -> ([Edge], Int)) -> String -> Int
p107p f str = totalWeight mat - mstWeight
    where mat = parseMatrix str
          (_, mstWeight) = f mat

parseMatrix :: String -> AdjMatrix
parseMatrix str = AdjMatrix nVertices (array bounds $ zip indices rows)
    where rowLines = lines str
          nVertices = length rowLines
          indices = map Vertex [1..nVertices]
          bounds = (Vertex 1, Vertex nVertices)
          parseVal "-" = 0
          parseVal v = read v
          parseLine = array bounds . zip indices . map parseVal . splitOn ','
          rows = map parseLine rowLines

-- Simple implementation of Prim's algorithm, searching the adjacency
-- matrix directly.
--
-- Complexity: O(|V|^2)
prims :: AdjMatrix -> ([Edge], Int)
prims mat = (edges, sum weights)
    where
      u0:initOut       = vertices mat
      init             = ((Set.singleton u0), (Set.fromList initOut))
      (weights, edges) = unzip $ unfoldr primStep init
      primStep :: (Set Vertex, Set Vertex)
                  -> Maybe ((Int, Edge), (Set Vertex, Set Vertex))
      primStep (inV, outV)
          | Set.null outV = Nothing
          | otherwise     = Just (e, (inV', outV'))
          where
            inNearest       = shortestEdgesIn mat outV $ Set.elems inV
            e@(w, Edge u v) = minimumBy (compareWith fst) inNearest
            inV'            = Set.insert v inV
            outV'           = Set.delete v outV

-- |Find the shortest possible edges from vertices in us to those in vs.
shortestEdgesIn :: AdjMatrix -> Set Vertex -> [Vertex] -> [(Int, Edge)]
shortestEdgesIn mat vs us = [(w, Edge u v) | (u,(v,w)) <- shortest]
    where
      possV = neighborsIn mat vs
      shortest = mapMaybe (maybePair $ nearestNeighbor . possV) us

nearestNeighbor :: [(Vertex, Int)] -> Maybe (Vertex, Int)
nearestNeighbor [] = Nothing
nearestNeighbor nl = Just $ minimumBy (compareWith snd) nl

-- Prim's algorithm, using a priority queue for faster lookup.
--
-- With the binomial heap used here, I think this should be 
-- O(|E| log |V|) as for a binary heap implementation, but 
-- I haven't worked it out.
type PrimPQ = MinPQueue Int Edge

primPQ :: AdjMatrix -> ([Edge], Int)
primPQ mat = (edges, sum weights)
    where
      u0:rest = vertices mat
      initOut = (Set.fromList rest)
      initQ   = maybeEnqueue mat initOut PQ.empty u0
      (weights, edges) = unzip $ unfoldr primStep (initOut, initQ)
      primStep :: (Set Vertex, PrimPQ)
               -> Maybe ((Int, Edge), (Set Vertex, PrimPQ))
      primStep (outV, pq)
          | PQ.null pq = Nothing
          | otherwise  = Just (e, (outV', pq'))
              where
                (e@(w, Edge u v), pqDel) = PQ.deleteFindMin pq
                outV' = Set.delete v outV
                pqAdd = foldl (maybeEnqueue mat outV') pqDel [u,v]
                pq'   = updatePQ mat outV' pqAdd

-- remove any leading stale elts from the queue and recalculate
updatePQ :: AdjMatrix -> Set Vertex -> PrimPQ -> PrimPQ
updatePQ mat outV oldQ = PQ.union remain (PQ.fromList recalc)
    where (stale, remain) =
              PQ.span (\(Edge _ v) -> Set.notMember v outV) oldQ
          recalc = shortestEdgesIn mat outV [u | (_, Edge u _) <- stale]

-- suitable for folding, with a PrimPQ accumulator and [Vertex]
maybeEnqueue :: AdjMatrix -> Set Vertex -> PrimPQ -> Vertex -> PrimPQ
maybeEnqueue mat outV pq u =
    case nearestNeighbor $ neighborsIn mat outV u of
      Nothing -> pq
      Just (v, w) -> PQ.insert w (Edge u v) pq

-- utility code

compareWith :: (Ord a, Ord b) => (a -> b) -> a -> a -> Ordering
compareWith f v1 v2 = compare (f v1) (f v2)

maybePair :: (a -> Maybe b) -> a -> Maybe (a,b)
maybePair f a = case f a of
                  Nothing -> Nothing
                  Just bv -> Just (a,bv)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim l0 = unfoldr splitter l0
    where splitter []   = Nothing
          splitter list = 
              case break (== delim) list of
                (head, []) -> Just (head, [])
                (head, _:tail) -> Just (head, tail)

-- 21 ms, not bad!
main :: IO ()
main = do
  str <- readFile "network.txt"
  print $ p107p primPQ str
