{-# LANGUAGE TypeApplications #-}
import Tree (Tree, fromList, inOrder, preOrder, postOrder, levelOrder)
import AVLTree (AVLTree)
import BinaryTree (BinaryTree)
import System.Environment (getArgs)
import Data.List (nub)
import Data.Function ( (&) )
import Control.Monad (guard, (=<<), forM_)
import qualified StatefulTree as St (print, insert, remove, runTreeM)

-- Old main usages for former courses
-- ----------------------------------

-- nums :: [Integer]
-- nums = [45, 20, 60, 10, 30, 55, 70, 15, 25, 40]
-- nums2 :: [Integer]
-- nums2 = [35, 20, 65, 10, 25, 45, 70, 15, 30, 40, 55]

-- t :: AVLTree Integer
-- t = fromList nums

-- t2 :: AVLTree Integer
-- t2 = fromList nums2

-- bruteForce = do
--     a <- nums'
--     b <- nums'
--     c <- nums'
--     d <- nums'
--     e <- nums'
--     guard $ 35 `elem` [a,c,d]
--     guard $ 65 `elem` [a,c,d]
--     let t' = t & add a & remove b & add c & add d & remove e
--     return ((a,b,c,d,e), t')
--     where nums' = nub $ nums ++ nums2

-- main = do
--     let ts = map fst $ filter ((==t2) . snd) bruteForce
--     print ts


-- main = St.runTreeM (singleton 5 :: AVLTree Integer) $ do
--     St.insert 3
--     St.insert 13
--     St.insert 2
--     St.insert 4
--     St.insert 8
--     St.insert 15
--     St.insert 1
--     St.insert 6
--     St.insert 12
--     St.insert 14
--     St.insert 16
--     St.insert 9
--     St.remove 4
--     St.insert 11
--     St.remove 5
--     St.print

main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Usage: <exe> <treetype> [values...]"
        (treeType:values) ->
            case treeType of
                "avl" -> go $ (fromList @AVLTree @Int) (read <$> values)
                "binary" -> go $ (fromList @BinaryTree @Int) (read <$> values)
                _ -> putStrLn "Bad tree type: valid types are 'avl' | 'binary'"
    where
        go :: (Tree t, Show (t a), Show a) => t a -> IO ()
        go t = do
            putStrLn "Tree"
            putStrLn (replicate 40 '=')
            print t
            putStrLn "Traversals"
            putStrLn (replicate 40 '=')
            putStrLn "Traverse in order (left, root, right)"
            print $ inOrder t
            putStrLn "Traverse pre order (root, left right)"
            print $ preOrder t
            putStrLn "Traverse post order (left, right, root)"
            print $ postOrder t
            putStrLn "Traverse level order (levels, top to bottom)"
            print $ levelOrder t