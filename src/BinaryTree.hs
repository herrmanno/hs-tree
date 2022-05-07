module BinaryTree (BinaryTree) where

import Data.List (intercalate)
import Tree (Tree(..))

data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)

instance Show a => Show (BinaryTree a) where
    show node = go node []
        where
            go n@(Node a l r) depth = intercalate "\n"
                [ prefix depth ++ show a
                , go l (depth ++ [True])
                , go r (depth ++ [False])
                ]
            go Empty depth =  prefix depth ++ "○"
            prefix [] = ""
            prefix n = concatMap (\x -> if x then "| " else "  ") (init n) ++ "⌞ "

instance Tree BinaryTree where
    empty = Empty
    singleton a = Node a Empty Empty
    null t = case t of { Empty -> True; _ -> False }
    value t = case t of { Empty -> Nothing; Node a _ _ -> Just a }
    left t = case t of { Empty -> Nothing; Node _ l _ -> Just l }
    right t = case t of { Empty -> Nothing; Node _ _ r -> Just r }
    insert Empty a = singleton a
    insert n@(Node v l r) a =
        case a `compare` v of
            LT -> Node v (insert l a) r
            GT -> Node v l (insert r a)
            _  -> n
    delete = error "Not supported"