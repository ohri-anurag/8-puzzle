module AStar(
  astar
) where

import Data.Int
import Data.Maybe
import Data.Ord
import qualified MinPQ as PQ
import qualified TreeSetBalanced as T

-- There are two types of nodes here:
--   1. The actual node with type 'a'
--   2. The wrapper node used by the astar algorithm with type 'Wrapper a'. It serves
--      to store the parent of the current node.
data Wrapper a =
  Wrapper { val :: a                      -- ^ The actual node.
          , parent :: (Maybe (Wrapper a)) -- ^ Wrapper node of the parent of the actual node
          , g :: Int                      -- ^ The distance of the current node from the starting node.
          , h :: Int                      -- ^ The heuristically calculated distance of the current node from the goal node.
          }
  deriving Show

-- Use both g and h to order nodes
instance Eq (Wrapper a) where
  x == y = (g x + h x) == (g y + h y)

instance Ord (Wrapper a) where
  compare (Wrapper _ _ g1 h1) (Wrapper _ _ g2 h2) = compare (g1 + h1) (g2 + h2)

astar :: (Eq a) => a
  -> a            -- ^ The starting node.
  -> (a -> [a])   -- ^ The goal node.
  -> (a -> Int)   -- ^ Function to generate neighbors of the current node.
  -> (a -> Int64) -- ^ Function that returns an Int64 value that uniquely identifies a node.
  -> [a]          -- ^ A list of nodes, representing the shortest path from the starting node to the goal node.
astar begin final generateNeighbors heuristic id =
  listNodes $ astarHelper (Wrapper begin Nothing 0 (heuristic begin)) PQ.empty T.empty
  where
    -- 'pq' is a MinPQ that stores the nodes according to the sum of their h and g values.
    -- 'set' is TreeSet stores the id representations of all the nodes that have been visited. By visited, we mean that they have been pushed into pq.
    astarHelper node pq set
      | (val node) == final = node
      | otherwise           = astarHelper node' pq' set'
        where
          -- Generate the neighbour nodes, but filter out the parent of the current node.
          neighborNodes = case parent node of
            Just par -> filter (val par /= ) $ generateNeighbors (val node)
            Nothing -> generateNeighbors (val node)

          -- Filter out the nodes that have already been visited.
          newNodes = filter (\n ->
                               not (T.contains set (id n))
                            ) $
                       neighborNodes

          -- Generate a new set by inserting the above visited nodes.
          set' = foldr (\n t -> T.insert (id n) t) set newNodes

          -- Generate wrapper nodes from newNodes
          wrapperNodes = map (\n ->
                            Wrapper n (Just node) (1 + g node) (heuristic n)
                         ) newNodes

          -- Generate a new pq by inserting the wrapper nodes, and then removing the next wrapper node to be processed.
          (node', pq') = fromJust . PQ.delMin $ foldr PQ.insert pq wrapperNodes

-- | This function generates a list by moving from node to its parent, until it reaches the node that had no parent.
listNodes :: Wrapper a -> [a]
listNodes node = helper (Just node) []
  where
    helper Nothing xs     = xs
    helper (Just node) xs = helper (parent node) (val node : xs)
