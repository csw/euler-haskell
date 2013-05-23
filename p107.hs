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

neighbors :: AdjMatrix -> Vertex -> [(Vertex, Int)]
neighbors (AdjMatrix deg mat) u =
    filter conn $ assocs (mat ! u)
    where conn (v, dist) = dist /= 0

edgeWeight :: AdjMatrix -> Edge -> Int
edgeWeight (AdjMatrix _ mat) (Edge u v) = (mat ! u) ! v

totalWeight :: AdjMatrix -> Int
totalWeight (AdjMatrix deg mat) = sum . map vWeight $ assocs mat
    where vWeight (u, adj) = sum $ map ((adj !) . Vertex) [((vid u)+1)..deg]

-- Parse a graph, find the MST, and compute the savings
p107 :: String -> Int
p107 str = totalWeight mat - mstWeight
    where mat = p107_parseMatrix str
          (_, mstWeight) = p107_prims mat

p107_splitter :: (Eq a) => a -> [a] -> Maybe ([a], [a])
p107_splitter delim [] = Nothing
p107_splitter delim list =
    case break (== delim) list of
      (head, []) -> Just (head, [])
      (head, _:tail) -> Just (head, tail)

p107_splitOn :: (Eq a) => a -> [a] -> [[a]]
p107_splitOn = unfoldr . p107_splitter

p107_parseMatrix :: String -> AdjMatrix
p107_parseMatrix str =
    let rowLines = lines str
        nVertices = length rowLines
        indices = map Vertex [1..nVertices]
        dims = (Vertex 1, Vertex nVertices)
        parseVal v = case v of "-" -> 0
                               _ -> read v
        parseLine = array dims . zip indices . map parseVal . p107_splitOn ','
        rows = map parseLine rowLines
    in AdjMatrix nVertices (array dims $ zip indices rows)

p107_prims :: AdjMatrix -> ([Edge], Int)
p107_prims mat = (edges, foldl (+) 0 $ map (edgeWeight mat) edges)
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
      outNeighbors u =
          [(v,dv) | (v,dv) <- neighbors mat u, Set.member v outV]
      nearestOut u =
          case outNeighbors u of
            [] -> Nothing
            nl -> Just $ minimumBy (\(_, d1) (_, d2) -> compare d1 d2) nl
      inNearest =
          [(u, fromJust n) | u <- Set.elems inV, let n = nearestOut u, isJust n]
      (u, (v,_)) = minimumBy (\(_,(_,d1)) (_,(_,d2)) -> compare d1 d2)
                   inNearest
