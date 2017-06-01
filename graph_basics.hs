import Data.List
import Control.Monad

stringToList :: String -> [Int]
stringToList l = map (read) (words l)

parseLinkInfo inputs = map (stringToList) inputs

-- Generate a graph with 'size' nodes and no link
emptyGraph 1    = [[]]
emptyGraph size = (emptyGraph (size - 1)) ++ [[]]

newGraphR i nodeCount links | i == -1 = (newGraphR (i+1) nodeCount links)
                       | i < nodeCount = (getConnections (filteredLinks i)) : (newGraphR (i+1) nodeCount links)
                       | otherwise = [] where
  filteredLinks x = sort (filter ((==x).head) links)
  getConnections []   = []
  getConnections (t:ts) = (t!!1) : (getConnections ts)

-- Receives a Integer number of nodes and a list of tuples for the links
newGraph linkCount links = newGraphR (-1) linkCount links

-- Given a graph and two nodes index, returns whether they share a link
hasConnection graph a b = b `elem` (graph!!a)

-- First line: nodeCount and linkCount;
-- Nth lines: A B -> link from A to B
-- Make sure to duplicate entries if your graph is not directed
main = do
  input <- getLine
  let firstLine = stringToList input
  let nodeCount = (firstLine!!0)
  let linkCount = (firstLine!!1)

  inputs <- replicateM linkCount getLine
  let links = parseLinkInfo inputs

  let graph = newGraph nodeCount links

  putStrLn $ show graph
