import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)] 
type AdjList = [(City,[(City,Distance)])]

-- tranforms a list in a list with only unique items
-- we made it because we didnt know nub existed
rmDoubles :: [String] -> [String]
rmDoubles [] = []
rmDoubles (h:t)
 | notElem h t = h : rmDoubles t
 | otherwise = rmDoubles t

-- FUNC1
-- returns all the cities in the graph

cities :: RoadMap -> [City]
cities rm = Data.List.nub ( [city1 | (city1, _, _) <- rm] ++ [city2 | (_, city2, _) <- rm])

-- FUNC 2
-- returns a boolean indicating whether two cities are linked directly

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm city1 city2 = any (\(x, y, d) -> (x==city1 && y==city2) || (x==city2 && y==city1)) rm

-- FUNC 3
-- returns a just value with the distance between two cities connected directly, given two city names, and Nothing otherwise.

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((city1_,city2_,distance_):xs) city1 city2
    | (city1_ == city1 && city2_ == city2)||(city1_ == city2 && city2_ == city1) = Just (distance_)
    | otherwise = distance xs city1 city2

-- FUNC 4
-- returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent rm city = [(x,y) | (c, x, y) <- rm, c == city] ++ [(x,y) | (x, c, y) <- rm, c == city]



-- FUNC 5
-- returns the sum of all individual distances in a path between two cities in a Just value

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance rm (x:y:xs) =
    case distance rm x y of
        Nothing -> Nothing
        Just dist ->
            case pathDistance rm (y:xs) of
                Nothing -> Nothing
                Just totalDist -> Just (dist + totalDist)


-- FUNC 6
-- returns the names of the cities with the
--highest number of roads connecting to them (i.e. the vertices with the highest degree) highestDegree

highestDegree :: RoadMap -> Int
highestDegree rm = maximum [length (adjacent rm c) | (c,x,y) <- rm]

rome :: RoadMap -> [City]
rome rm = Data.List.nub [city | (city, y, d) <- rm, length (adjacent rm city) == h]
    where h = highestDegree rm

-- FUNC 7
-- returns a boolean indicating whether all the cities in the graph are connected in the roadmap

dfs :: RoadMap -> City -> [City] -> [City]
dfs rm city visited
 | city `elem` visited = visited --see if city is already in visited
 | otherwise = foldl (\acc (nextCity, _) -> dfs rm nextCity acc) (city:visited) (adjacent rm city)

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = True
isStronglyConnected rm =
    let cityList = cities rm
        startCity = head cityList
        visited = dfs rm startCity []
    in length visited == length cityList

-- FUNC 8
-- converts the RoadMap into [(City,[(City,Distance)])] (a lisst of tuples with every city and a list of all the adjacent cities to this city. and their distance)
convert :: RoadMap -> [(City,[(City,Distance)])]
convert rm = [(city1,adjacent rm city1) | city1 <- cities rm]


getNeighbors :: AdjList -> City -> [(City, Distance)]
getNeighbors adjList city = case lookup city adjList of
    Just neighbors -> neighbors
    Nothing -> []

-- Shortest Paths function to return all paths with minimum distance
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm start end
    | start == end = [[start]]
    | otherwise =
        let adjList = convert rm
            initialQueue = [(start, 0, [start])]
            visited = []
            bfs [] _ paths _ = Data.List.nub paths
            bfs queue visited paths minDist
                | null queue =  Data.List.nub paths
                | otherwise =
                    let (currentCity, currentDist, currentPath) = head queue
                        restQueue = tail queue
                    in if currentCity == end
                       then
                           -- If we reach the end, check if we have the minimum path
                           let newPaths
                                 | currentDist == minDist = currentPath : paths
                                 | currentDist < minDist = [currentPath]
                                 | otherwise = paths
                           in bfs restQueue visited newPaths (min minDist currentDist)
                       else if currentCity `elem` visited
                            then bfs restQueue visited paths minDist
                            else
                                let neighbors = getNeighbors adjList currentCity
                                    updatedQueue = foldl (updateQueue currentDist currentPath) restQueue neighbors
                                in bfs (restQueue ++ updatedQueue) (currentCity : visited) paths minDist

            -- Update queue and add path if the current path is shortest or equal to the shortest
            updateQueue currentDist currentPath q (neighbor, dist) =
                let newDist = currentDist + dist
                    newPath = currentPath ++ [neighbor]
                in (neighbor, newDist, newPath) : filter (\(c, _, _) -> c /= neighbor) q

        in bfs initialQueue visited [] maxBound
        
--FUNC 9
-- returns a solution of the Traveling Salesman Problem (TSP).

-- Finds the nearest unvisited neighbor from the current city
findNearestNeighbor :: AdjList -> City -> [City] -> Maybe (City, Distance)
findNearestNeighbor adjList currentCity unvisited =
    let neighbors = getNeighbors adjList currentCity
        validNeighbors = filter (\(c, _) -> c `elem` unvisited) neighbors
    in if null validNeighbors
       then Nothing
       else Just (Data.List.minimumBy (\(_, d1) (_, d2) -> compare d1 d2) validNeighbors)

-- Nearest neighbor algorithm to find the path
nearestNeighbor :: AdjList -> City -> [City] -> Path -> Path
nearestNeighbor adjList currentCity unvisited path
    | null unvisited =
        case lookup (head path) (getNeighbors adjList currentCity) of
            Nothing -> []  -- If there's no path back to start, return an empty path
            Just _  -> path ++ [head path]  -- Otherwise, return to start to complete the tour
    | otherwise =
        case findNearestNeighbor adjList currentCity unvisited of
            Nothing -> []  -- If no valid neighbor is found, return an empty path (no valid TSP path)
            Just (nextCity, _) ->
                nearestNeighbor adjList nextCity (filter (/= nextCity) unvisited) (path ++ [nextCity])


-- TSP function using the nearest neighbor approach
travelSales :: RoadMap -> Path
travelSales rm =
    let adjList = convert rm
        startCity = head (cities rm)
        allCities = cities rm
        path = nearestNeighbor adjList startCity (filter (/= startCity) allCities) [startCity]
    in if length path == length allCities + 1 && last path == startCity
       then path
       else []


tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap
gTest4 = [("0", "1", 1), ("1", "3", 1), ("2", "3",1),("0","2",1)]

gTest5 :: RoadMap
gTest5 = [("A", "B", 5), ("A", "C", 10), ("A", "D", 8),
          ("B", "C", 3), ("B", "D", 7),
          ("C", "D", 2)]

gTest6 :: RoadMap
gTest6 = [("X", "Y", 2), ("Y", "Z", 3), ("Z", "W", 4), ("W", "X", 5)]

gTest7 :: RoadMap
gTest7 = [("1", "2", 4), ("1", "3", 6), ("1", "4", 8), ("2", "3", 2),
          ("2", "4", 3), ("3", "5", 5), ("4", "5", 7), ("3", "6", 4),
          ("5", "6", 1), ("4", "6", 6)]

gTest8 :: RoadMap
gTest8 = [("M", "N", 5), ("N", "O", 10), ("O", "M", 6),  -- First subgraph
          ("P", "Q", 3), ("Q", "R", 4), ("R", "P", 2)]   -- Second subgraph

gTest9 :: RoadMap
gTest9 = [("A", "B", 3), ("A", "C", 2), ("B", "C", 1),
          ("C", "D", 4), ("B", "D", 6), ("A", "D", 5)]

gTest10 :: RoadMap
gTest10 = [("1", "2", 2), ("2", "3", 2), ("3", "4", 2), ("4", "5", 2)]


-- Test with gTest1
-- Expected: Shortest path(s) between each city pair (if connected)
test1_1 = shortestPath gTest1 "0" "5"  -- Path from "0" to "5", expected result(s): [["0","7","6","5"], ["0","1","2","5"]]
test1_2 = shortestPath gTest1 "7" "4"  -- Path from "7" to "4", expected result(s): [["7","6","5","4"], ["7","8","2","3","4"]]
test1_3 = shortestPath gTest1 "8" "3"  -- Path from "8" to "3", expected result(s): [["8","2","3"]]
test1_4 = shortestPath gTest1 "1" "3"  -- Path from "1" to "3", expected result(s): [["1","2","3"]]
test1_5 = shortestPath gTest1 "0" "8"  -- Path from "0" to "8", expected result(s): [["0","7","8"]]

-- Test with gTest2
-- Expected: Shortest path(s) between each city pair (if connected)
test2_1 = shortestPath gTest2 "0" "3"  -- Path from "0" to "3", expected result(s): [["0","3"]]
test2_2 = shortestPath gTest2 "1" "2"  -- Path from "1" to "2", expected result(s): [["1","2"]]
test2_3 = shortestPath gTest2 "2" "0"  -- Path from "2" to "0", expected result(s): [["2","0"]]

-- Test with gTest3 (Unconnected graph)
-- Expected: Shortest path(s) for only reachable cities
test3_1 = shortestPath gTest3 "0" "1"  -- Path from "0" to "1", expected result(s): [["0","1"]]
test3_2 = shortestPath gTest3 "2" "3"  -- Path from "2" to "3", expected result(s): [["2","3"]]
test3_3 = shortestPath gTest3 "0" "3"  -- Path from "0" to "3", expected result: [] (no path exists)

-- Test with gTest4
-- Expected: Shortest path(s) between each city pair (if connected)
test4_1 = shortestPath gTest4 "0" "3"  -- Path from "0" to "3", expected result(s): [["0","1","3"], ["0","2","3"]]
test4_2 = shortestPath gTest4 "1" "2"  -- Path from "1" to "2", expected result(s): [["1","3","2"]]
test4_3 = shortestPath gTest4 "0" "1"  -- Path from "0" to "1", expected result(s): [["0","1"]]
