import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)] -- original version

-- tranforms a list in a list with only unique items
rmDoubles :: [String] -> [String]
rmDoubles [] = []
rmDoubles (h:t)
 | notElem h t = h : rmDoubles t 
 | otherwise = rmDoubles t

-- FUNC1
-- returns all the cities in the graph

auxcities :: RoadMap -> [City]
auxcities [] = []
auxcities ((city1,city2,_):xs) =  city1 : city2 : auxcities xs

cities :: RoadMap -> [City]
cities rm = rmDoubles(auxcities rm)
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
pathDistance = undefined

--FUNC 6
-- returns the names of the cities with the
--highest number of roads connecting to them (i.e. the vertices with the highest degree) highestDegree
highestDegree :: RoadMap -> Int
highestDegree rm = maximum [length (adjacent rm c) | (c,x,y) <- rm] 

rome :: RoadMap -> [City]
rome rm = rmDoubles [city | (city, y, d) <- rm, length (adjacent rm city) == h]
    where h = highestDegree rm

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]


