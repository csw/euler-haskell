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

maybePair :: (a -> Maybe b) -> a -> Maybe (a,b)
maybePair f a = case f a of
                  Nothing -> Nothing
                  Just bv -> Just (a,bv)

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

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim l0 = unfoldr splitter l0
    where splitter []   = Nothing
          splitter list = 
              case break (== delim) list of
                (head, []) -> Just (head, [])
                (head, _:tail) -> Just (head, tail)

parseMatrix :: String -> AdjMatrix
parseMatrix str =
    let rowLines = lines str
        nVertices = length rowLines
        indices = map Vertex [1..nVertices]
        dims = (Vertex 1, Vertex nVertices)
        parseVal "-" = 0
        parseVal v = read v
        parseLine = array dims . zip indices . map parseVal . splitOn ','
        rows = map parseLine rowLines
    in AdjMatrix nVertices (array dims $ zip indices rows)

prims :: AdjMatrix -> ([Edge], Int)
prims mat = (edges, sum $ map (edgeWeight mat) edges)
    where x:xs  = vertices mat
          init  = ((Set.singleton x), (Set.fromList xs))
          edges = unfoldr (prims' mat) init

prims' :: AdjMatrix
       -> (Set Vertex, Set Vertex)
       -> Maybe (Edge, (Set Vertex, Set Vertex))
prims' mat (inV, outV)
    | Set.null outV        = Nothing
    | otherwise            =
          Just (Edge u v, (Set.insert v inV, Set.delete v outV))
    where
      outNeighbors = neighborsIn mat outV
      nearest [] = Nothing
      nearest nl = Just $ minimumBy (\(_, d1) (_, d2) -> compare d1 d2) nl
      inNearest =
          mapMaybe (maybePair $ nearest . outNeighbors) $ Set.elems inV
      (u, (v,_)) = minimumBy (\(_,(_,d1)) (_,(_,d2)) -> compare d1 d2)
                   inNearest

-- Prim's algorithm with PQ
--
-- PQ will have one entry per vertex in Vnew (a.k.a. inV)
--  * PQ entries: key distance, elt (u,v) for the nearest v in outV
--  * break ()
--  * create outV' with new status of v
--  * add binding for v
--  * update binding for u

-- data QEntry = QEntry Int Vertex Vertex
--               deriving (Eq, Show, Ord)

-- from                :: QEntry -> Vertex
-- from (QEntry _ u _) = u

prim_pq :: AdjMatrix -> ([Edge], Int)
prim_pq mat = (edges, sum $ map (edgeWeight mat) edges)
    where u0:rest = vertices mat
          outV    = (Set.fromList rest)
          Just (v0, w0) = nearestNeighbor $ neighborsIn mat outV u0
          initQ   = PQ.singleton w0 $ Edge u0 v0
          init    = (outV, initQ)
          edges   = unfoldr (prim_pq' mat) init

type PrimPQ = MinPQueue Int Edge

prim_pq' :: AdjMatrix
          -> (Set Vertex, PrimPQ)
          -> Maybe (Edge, (Set Vertex, PrimPQ))
prim_pq' mat (outV, pq)
    | PQ.null pq = Nothing
    | otherwise  =
        Just (edge, (outV', updatePQ mat outV' pq'))
      where ((w, edge@(Edge u v)), pqDel) =
                PQ.deleteFindMin pq
            outV' = Set.delete v outV
            pq' = foldl (maybeEnqueue mat outV') pqDel [u,v]

-- remove any leading stale elts from the queue and recalculate
updatePQ :: AdjMatrix -> Set Vertex -> PrimPQ -> PrimPQ
updatePQ mat outV oldQ = PQ.union remain recalc
    where (stale, remain) =
              PQ.span (\(Edge _ v) -> Set.notMember v outV) oldQ
          staleU = [u | (_, Edge u _) <- stale]
          recalcN = mapMaybe (maybePair (nearestNeighbor . neighborsIn mat outV)) staleU
          recalc = PQ.fromList [(dist, Edge u v) | (u,(v,dist)) <- recalcN]

-- suitable for folding, with a PrimPQ accumulator and [Vertex]
maybeEnqueue :: AdjMatrix -> Set Vertex -> PrimPQ -> Vertex -> PrimPQ
maybeEnqueue mat outV pq u =
    case nearestNeighbor $ neighborsIn mat outV u of
      Nothing -> pq
      Just (v, w) -> PQ.insert w (Edge u v) pq

nearestNeighbor :: [(Vertex, Int)] -> Maybe (Vertex, Int)
nearestNeighbor [] = Nothing
nearestNeighbor nl = Just $ minimumBy (compareWith snd) nl

compareWith :: (Ord a, Ord b) => (a -> b) -> a -> a -> Ordering
compareWith f v1 v2 = compare (f v1) (f v2)

-- 21 ms, not bad!
main :: IO ()
main = do
  str <- readFile "network.txt"
  print $ p107p prim_pq str
