{-# LANGUAGE FlexibleContexts #-}
module StatefulTree (
    StatefulTree,
    StatefulTreeM,
    runTree,
    runTreeM,
    insert,
    remove,
    StatefulTree.print
) where

import qualified Tree as T
import Control.Monad.State 
import Control.Monad.Identity

type StatefulTreeM t a m = StateT (t a) m ()
type StatefulTree t a = StatefulTreeM t a Identity

runTree :: (T.Tree t) => t a -> StatefulTree t a -> t a
runTree = flip execState

runTreeM :: (Monad m, T.Tree t) => t a -> StatefulTreeM t a m -> m (t a)
runTreeM = flip execStateT

insert :: (Monad m, Ord a, T.Tree t) => a -> StatefulTreeM t a m
insert a = modify (T.add a)

remove :: (Monad m, Ord a, T.Tree t) => a -> StatefulTreeM t a m
remove a = modify (T.remove  a)

print :: (MonadIO m, Show (t a), T.Tree t) => StatefulTreeM t a m
print = get >>= (liftIO . Prelude.print)