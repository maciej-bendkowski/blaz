{-# LANGUAGE BangPatterns #-}
{- |
 - Module       : Memo
 - Description  : Infinite memorization tree structure
 - Copyright    : (c) Maciej Bendkowski
 -
 - Maintainer   : maciej.bendkowski@gmail.com
 - Stability    : experimental
-}
module Memo (
    Tree(..), idx, nats, toList
) where

    -- | An infinite binary tree data structure
    -- with additional functor capabilities
    data Tree a = Tree (Tree a) a (Tree a)
    instance Functor Tree where
        fmap f (Tree l x r) = Tree (fmap f l) (f x) (fmap f r)
    
    -- | Tree indexer
    idx :: Tree a -> Int -> a
    idx (Tree _ x _) 0 = x
    idx (Tree l _ r) k = case (k-1) `divMod` 2 of
        (k', 0) -> idx l k'
        (k', _) -> idx r k' 

    -- | Natural numbers represented in an
    -- infinite binary tree
    nats :: Tree Int
    nats = f 0 1 where
        f :: Int -> Int -> Tree Int
        f !k !s = Tree (f l s') k (f r s') where
            l = k + s
            r = l + s
            s' = s * 2
    
    -- | Tree to list converter
    toList :: Tree a -> [a]
    toList ts = map (idx ts) [0..]
