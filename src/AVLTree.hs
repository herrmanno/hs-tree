{-# LANGUAGE CPP #-}
-- #define DEBUG

module AVLTree (
    AVLTree,
    singleton,
    empty,
    balanceFactor,
) where

import Prelude hiding (succ,min,map)
import Control.Monad (MonadPlus)
import Control.Applicative ((<|>), Alternative)
import qualified Control.Applicative as Ap
import Data.List (intercalate)
import Data.Function ( (&) )
import Debug.Trace (trace)
import Tree (Tree(..), height)

data AVLTree a = Node a (AVLTree a) (AVLTree a) | Empty

instance (Show a) => Show (AVLTree a) where
    show node = go node []
        where
            go n@(Node a l r) depth = intercalate "\n"
                [ prefix depth ++ show a ++ " (" ++ show (balanceFactor n) ++ ")"
                , go l (depth ++ [True])
                , go r (depth ++ [False])
                ]
            go Empty depth =  prefix depth ++ "○"
            prefix [] = ""
            prefix n = concatMap (\x -> if x then "| " else "  ") (init n) ++ "⌞ "

instance (Eq a) => Eq (AVLTree a) where
    Node a l r == Node b l' r' = a == b && l == l' && r == r'
    Empty == Empty = True
    _ == _ = False

instance Functor AVLTree where
    fmap f (Node a l r) = Node (f a) (f <$> l) (f <$> r)
    fmap f Empty = Empty

instance Applicative AVLTree where
    pure = singleton
    Node f _ _ <*> Node a l r = Node (f a) (f <$> l) (f <$> r) 
    _ <*> _ = Empty

instance Alternative AVLTree where
    empty = Empty
    Empty <|> r = r
    l <|> Empty = l

instance Monad AVLTree where
    return = pure
    Empty >>= f = Empty
    Node a l r >>= f = case f a of
        Node b _ _ -> Node b (l >>= f) (r >>= f)
        _ -> Empty

instance Tree AVLTree where
    empty = Empty
    singleton a = Node a Empty Empty
    null t = case t of { Empty -> True; _ -> False }
    value t = case t of { Empty -> Nothing; Node a _ _ -> Just a }
    left t = case t of { Empty -> Nothing; Node _ l _ -> Just l }
    right t = case t of { Empty -> Nothing; Node _ _ r -> Just r }
    insert Empty a = singleton a
    insert n@(Node v l r) a =
        case a `compare` v of
            LT -> balance $ Node v (insert l a) r
            GT -> balance $ Node v l (insert r a)
            _  -> n
    delete Empty _ = Empty
    delete n@(Node v l r) a =
        case a `compare` v of
            LT -> balance $ Node v (delete l a) r
            GT -> balance $ Node v l (delete r a)
            _  -> case (l,r) of
                    (l'@Node{}, r'@Node{}) -> let s = succ n in balance $ Node s l (delete r' s)
                    _ -> l <|> r


map :: (AVLTree a -> b) -> AVLTree a -> [b]
map _ Empty = []
map f n@(Node _ l r) = f n : map f l ++ map f r

balanceFactor :: (Integral b) => AVLTree a -> b
balanceFactor Empty = 0
balanceFactor (Node _ l r) = height r - height l

balance :: AVLTree a -> AVLTree a
balance Empty = Empty
balance n@(Node a l r) = case balanceFactor n of
    -2 -> case balanceFactor l of
        1 -> rotateRight (Node a (rotateLeft l) r)
        _ -> rotateRight n
    2 -> case balanceFactor r of
        -1 -> rotateLeft (Node a l (rotateRight r))
        _ -> rotateLeft n
    _ -> n

rotateRight :: AVLTree a -> AVLTree a
rotateRight Empty = Empty
rotateRight (Node a (Node al ll lr) r) =
#ifdef DEBUG
    trace "Rotating right" $ Node al ll (Node a lr r)
#else
    Node al ll (Node a lr r)
#endif

rotateLeft :: AVLTree a -> AVLTree a
rotateLeft Empty = Empty
rotateLeft (Node a l (Node ar rl rr)) =
#ifdef DEBUG
    trace "Rotating left" $ Node ar (Node a l rl) rr
#else
    Node ar (Node a l rl) rr
#endif

succ :: AVLTree a -> a
succ (Node _ _ Empty) = error "no successor"
succ (Node _ _ r) = min r

min :: AVLTree a -> a
min (Node a Empty _) = a
min (Node a l _) = min l
