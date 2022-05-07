{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Tree 
    ( Tree (..)
    , fromList
    , toList
    , collect
    , inOrder
    , preOrder
    , postOrder
    , levelOrder
    , height
    ) where

import Prelude hiding (succ,min,map)
import Data.List (foldl')

class Tree t where
    -- | Creates an empty tree
    empty :: t a
    -- | Creates a tree with one element
    singleton :: a -> t a
    -- | Checks if a tree is empty
    null :: t a -> Bool
    -- | Inserts a node into a tree
    insert :: (Ord a) => t a -> a -> t a
    -- | Deletes a node from a tree
    delete :: (Ord a) => t a -> a -> t a
    -- | Extracts the root from a tree, if not empty
    value :: t a -> Maybe a
    -- | Extracts the left child from a tree, if not empty
    left :: t a -> Maybe (t a)
    -- | Extracts the right child from a tree, if not empty
    right :: t a -> Maybe (t a)
    -- | Flipped version of `insert`
    add :: (Ord a) => a -> t a -> t a
    add = flip insert
    -- | Flipped version of `remove`
    remove :: (Ord a) => a -> t a -> t a
    remove = flip delete

-- | Helper struct to generalize trees
data T t a = TEmpty | TNode a (t a) (t a)

-- | Extracts a node from a tree
toNode :: (Tree t) => t a -> Maybe (T t a)
toNode t = TNode <$> value t <*> left t <*> right t

-- | A (general) empty tree
pattern Empty <- (Tree.null -> True)

-- | A (general) non-empty tree
pattern Node a l r <- (toNode -> Just (TNode a l r))

-- | Creates a tree from a list
fromList :: (Tree t, Ord a) => [a] -> t a
fromList [] = empty
fromList (x:xs) = foldl' insert (singleton x) xs

-- | Creates a list from a tree in level order
toList :: Tree t => t a -> [a]
toList = levelOrder

-- | Creates a list from a tree in order
collect :: (Tree t) => t a -> [a]
collect = inOrder

-- | Traverse a tree in order left, root, right
inOrder :: (Tree t) => t a -> [a]
inOrder Empty = []
inOrder (Node a l r) = inOrder l ++ (a : inOrder r)

-- | Traverse a tree in order root, left, right
preOrder :: (Tree t) => t a -> [a]
preOrder Empty = []
preOrder (Node a l r) = a : preOrder l ++ preOrder r

-- | Traverse a tree in order left, right, root
postOrder :: (Tree t) => t a -> [a]
postOrder Empty = []
postOrder (Node a l r) = postOrder l ++ postOrder r ++ [a]

-- | Traverse a tree in levels
levelOrder :: (Tree t) => t a -> [a]
levelOrder node = go [node]
    where
        go [] = []
        go (Empty:xs) = go xs
        go ((Node a l r): xs) = a : go (xs ++ [l,r])

-- | Calculates the height of a tree
height :: (Tree t, Integral b) => t a -> b
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)
