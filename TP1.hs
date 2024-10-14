import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)] -- original version

cities :: RoadMap -> [City]
cities = undefined -- modifiy this line to implement the solution, for each exercise not solved, leave the function definition like this

--FUNC 2
--returns a boolean indicating whether two cities are linked directly
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm city1 city2 = any (\(x, y, d) -> (x==city1 && y==city2) || (x==city2 && y==city1)) rm

distance :: RoadMap -> City -> City -> Maybe Distance
distance = undefined

--FUNC 4
-- returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent rm city = [(x,y) | (c, x, y) <- rm, c == city] ++ [(x,y) | (x, c, y) <- rm, c == city]

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

rome :: RoadMap -> [City]
rome = undefined

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
