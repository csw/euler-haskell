import Data.Array.IArray
import Data.Array.Unboxed
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set


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

maybeZip :: [a] -> [Maybe b] -> [(a,b)]
maybeZip [] bs = []
maybeZip as [] = []
maybeZip (a:as) (Nothing:bs) = maybeZip as bs
maybeZip (a:as) ((Just b):bs) = (a,b) : maybeZip as bs

maybePair :: (a -> Maybe b) -> a -> Maybe (a,b)
maybePair f a = case f a of
                  Nothing -> Nothing
                  Just bv -> Just (a,bv)

neighborsIn :: AdjMatrix -> Vertex -> Set Vertex -> [(Vertex, Int)]
neighborsIn mat u vSet =
    mapMaybe (maybePair (distance mat u)) $ Set.elems vSet

edgeWeight :: AdjMatrix -> Edge -> Int
edgeWeight (AdjMatrix _ mat) (Edge u v) = (mat ! u) ! v

totalWeight :: AdjMatrix -> Int
totalWeight (AdjMatrix deg mat) = sum . map vWeight $ assocs mat
    where vWeight (u, adj) = sum $ map ((adj !) . Vertex) [((vid u)+1)..deg]

-- Parse a graph, find the MST, and compute the savings
-- = 259679
p107 :: String -> Int
p107 str = totalWeight mat - mstWeight
    where mat = p107_parseMatrix str
          (_, mstWeight) = p107_prims mat

p107_splitOn :: (Eq a) => a -> [a] -> [[a]]
p107_splitOn delim l0 = unfoldr splitter l0
    where splitter []   = Nothing
          splitter list = 
              case break (== delim) list of
                (head, []) -> Just (head, [])
                (head, _:tail) -> Just (head, tail)

p107_parseMatrix :: String -> AdjMatrix
p107_parseMatrix str =
    let rowLines = lines str
        nVertices = length rowLines
        indices = map Vertex [1..nVertices]
        dims = (Vertex 1, Vertex nVertices)
        parseVal "-" = 0
        parseVal v = read v
        parseLine = array dims . zip indices . map parseVal . p107_splitOn ','
        rows = map parseLine rowLines
    in AdjMatrix nVertices (array dims $ zip indices rows)

p107_prims :: AdjMatrix -> ([Edge], Int)
p107_prims mat = (edges, sum $ map (edgeWeight mat) edges)
    where x:xs  = vertices mat
          init  = ((Set.singleton x), (Set.fromList xs))
          edges = unfoldr (p107_prims' mat) init

p107_prims' :: AdjMatrix
            -> (Set Vertex, Set Vertex)
            -> Maybe (Edge, (Set Vertex, Set Vertex))
p107_prims' mat (inV, outV)
    | Set.null outV        = Nothing
    | otherwise            =
          Just (Edge u v, (Set.insert v inV, Set.delete v outV))
    where
      outNeighbors u = neighborsIn mat u outV
      nearest [] = Nothing
      nearest nl = Just $ minimumBy (\(_, d1) (_, d2) -> compare d1 d2) nl
      inNearest =
          mapMaybe (maybePair $ nearest . outNeighbors) $ Set.elems inV
      (u, (v,_)) = minimumBy (\(_,(_,d1)) (_,(_,d2)) -> compare d1 d2)
                   inNearest

-- 21 ms, not bad!
main :: IO ()
main = do
  str <- readFile "network.txt"
  print $ p107 str
