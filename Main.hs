module Main(
Node,
NodeSystem,
makeNode,
buildSystem,
reconnectIfBad,
findLastBad,
solve,
main
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Map

import Prelude hiding (lookup, foldl, filter)

-- |Just wraps a Map Int Node
type NodeSystem = Map Int Node

-- |A Node is comprised of its index, the next index and a Bool showing it was
-- reconnected or not. Any algorithm relaying on this data type should have access
-- to a Map or List where it can fetch Node references from both index and nextIndex
data Node = Node {  index :: Int,
                    nextIndex :: Int,
                    reconnected :: Bool } deriving (Show)

instance Ord Node where
  (Node indexA _ _) `compare` (Node indexB _ _) = indexA `compare` indexB

instance Eq Node where
  (Node indexA nextA _) == (Node indexB nextB _) = indexA == indexB && nextA == nextB

-- |Helper just to not instantiate a Node by hand.
makeNode :: Int -- ^ Node index
            -> Int -- ^ Node index this node points to
            -> (Int, Node) -- ^ As we are creating our node and will stuff them
                           -- into a Map Int Node, here is the association
makeNode i t = (i, Node { index = i, nextIndex = t, reconnected = False})

-- |Given a list of Int, transform into a NodeSystem
buildSystem :: [Int] -> NodeSystem
buildSystem inputList = fromList $ getZipList $ makeNode <$> ZipList [0..] <*> ZipList inputList

-- |Recursive function that given a pool of nodes (a NodeSystem) - possibly all nodes - will
-- walk over the possible path through nextIndex.
--
-- The intention here is to return the deepest bad node in starting from a given Node.
--
-- Returns Nothing if the deepest Node is actually a Good Node.
findLastBad :: NodeSystem -> Node -> Maybe Node
findLastBad _ (Node _ 0 _) = Nothing
findLastBad _ node@(Node i t _) | i == t = Just node
findLastBad pool node =
  case lookup (nextIndex node) pool of
    Nothing -> Just node
    Just n -> findLastBad (delete (index node) pool) n

-- |This function simply produces a new NodeSystem with the given Node updated with
-- reconnected = True and nextIndex = 0, given that the default behavior for deep Bad nodes
-- is just point them straight to a already known Good Node (Node 0 _ _).
reconnectIfBad :: NodeSystem -> Node -> NodeSystem
reconnectIfBad pool node
  | Just True <- alreadyChanged = pool
  | Just n <- badNode =
      update (\ _ ->  Just n { nextIndex = 0, reconnected = True} ) (index n) pool
  | otherwise = pool
  where node' = lookup (index node) pool
        alreadyChanged = (/=) <$> Just node <*> node'
        badNode = findLastBad pool node

-- |This is the code function that simply 'Map.foldl' over a 'NodeSystem' producing a new one
-- with all Good Nodes with minimum moves. Then it how many nodes where reconnected.
--
-- >> (solve $ buildSystem [1,2,1] ) == 1
solve :: NodeSystem -> Int
solve pool = size . filter reconnected $ foldl reconnectIfBad pool pool

main :: IO ()
main = trySolve `catch` handler

-- This just do the IO thing
trySolve :: IO ()
trySolve = do
  sytemSize <- getLine
  nodes <- replicateM (read sytemSize) getLine
  let rawNodes = Prelude.map read nodes
  let system = buildSystem rawNodes
  putStrLn $ show  $ solve system

handler :: IOError -> IO ()
handler _ = putStrLn "Whoops, had some trouble!"
