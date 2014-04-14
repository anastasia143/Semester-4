import Data.Maybe
import Data.List

data Graph v e = Graph [(Int,v)] [(Int,Int,e)]

-- vertexes = (number, current min dist)
numberAndDistance (a,b) = (a, Just b)

-- edges = (vertex 1, vertex 2, value)
value (_,_,c) = c

-- incident edge
findEdge x y list = find (\(a,b,_) -> x == a && y == b || x == b && y == a) list
-- vertex with min dist (between two)
findNearVertex (a1,b1) (a2,b2) = if b1 < b2 then (a1,b1) else (a2,b2)

-- set: connected undirected graph, root
-- get: [(vertex number, result min dist)]
dijkstra :: (Eq a, Ord a, Fractional a) => Graph v a -> Int -> [(Int, Maybe a)]                                    
dijkstra (Graph vs es) root = map numberAndDistance $ helper (map (\(a,_) -> (a, if a == root then 0 else 1/0)) vs) []
    where helper [] res = res
          helper notVisited visited = helper (map recount $ filter (/= minNotVisitedVertex) notVisited) (minNotVisitedVertex:visited)
                 where minNotVisitedVertex = foldl findNearVertex (head notVisited) (tail notVisited) -- vertex with min dist (between all)
                       recount (number, distance) = countHelper $ findEdge number (fst minNotVisitedVertex) es
                              where countHelper Nothing = (number, distance)
                                    countHelper (Just edge) | snd minNotVisitedVertex + value edge < distance = (number, snd minNotVisitedVertex + value edge)
                                                            | otherwise = (number, distance)
main = do
    -- example from wiki
    let g = Graph [(1,5),(2,4),(3,5),(4,6),(5,1),(6,2)] [(1,3,9),(1,2,7),(1,6,14),(2,3,10),(2,4,15),(6,3,2),(6,5,9),(3,4,11),(5,4,6)]
    putStrLn(show $ dijkstra g 1)