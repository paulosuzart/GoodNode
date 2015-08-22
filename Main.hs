{-|
Module      : Good Node
Description : impl for Hacker Rank

Used at blog post: <http://paulosuzart.github.io/blog/2015/08/21/how-much-functional-are-you/>
-}
module Main(
Node,
NodeSystem,
makeNodeAssoc,
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

-- |A Node is comprised of its index, the next index.--
-- Any algorithm relaying on this data type should have access
-- to a Map or List where it can fetch Node references from both index and nextIndex
data Node = Node {  index :: Int,
                    nextIndex :: Int } deriving (Show)

instance Ord Node where
  (Node indexA _) `compare` (Node indexB _) = indexA `compare` indexB

instance Eq Node where
  (Node indexA nextA) == (Node indexB nextB) = indexA == indexB && nextA == nextB

-- |Helper just to not instantiate a Node by hand.
makeNodeAssoc :: Int -- ^ Node index
            -> Int -- ^ Node index this node points to
            -> (Int, Node) -- ^ As we are creating our node and will stuff them
                           -- into a Map Int Node, here is the association
makeNodeAssoc i t = (i, Node { index = i, nextIndex = t})

-- |Given a list of Int, transform into a NodeSystem
buildSystem :: [Int] -> NodeSystem
buildSystem inputList = fromList $ getZipList $ makeNodeAssoc <$> ZipList [0..] <*> ZipList inputList

-- |Recursive function that given a pool of nodes (a NodeSystem) - possibly all nodes - will
-- walk over the possible path through nextIndex.
--
-- The intention here is to return the deepest bad node in starting from a given Node.
--
-- Returns Nothing if the deepest Node is actually a Good Node.
findLastBad :: NodeSystem -> Node -> Maybe Node
findLastBad _ (Node _ 0) = Nothing
findLastBad _ node@(Node i t) | i == t = Just node
findLastBad pool node =
  case lookup (nextIndex node) pool of
    Nothing -> Just node
    Just n -> findLastBad (delete (index node) pool) n

-- |This function simply recursively consume a list of node index.
-- Takes two accumulators, the acc that counts how many nods were changed,
-- and the system that is changed if needed ('Map.update').
reconnectIfBad :: Int -> NodeSystem -> [Int] -> Int
reconnectIfBad _ _ [] = 0
reconnectIfBad acc system (x:xs) =
  case findLastBad system node of
    Just n -> 1 + reconnectIfBad acc (reconnect n) xs
    _ -> reconnectIfBad acc system xs
    where node = system ! x
          reconnect n = update (\ _ ->  Just n { nextIndex = 0 } ) (index n) system

-- |This is the code function that simply 'Map.foldl' over a 'NodeSystem' producing a new one
-- with all Good Nodes with minimum moves. Then it how many nodes where reconnected.
--
-- >> (solve $ buildSystem [1,2,1] ) == 1
solve :: NodeSystem -> Int
solve pool = reconnectIfBad 0 pool $ keys pool

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
