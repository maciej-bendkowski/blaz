{- |
 - Module       : CommonUtils
 - Description  : Common utilities
 - Copyright    : (c) Maciej Bendkowski
 -
 - Maintainer   : maciej.bendkowski@gmail.com
 - Stability    : experimental
 -}
module CommonUtils (
    binom, catalan, leq, eq, geq
) where
    
    -- | Computes the binominal(n, k)
    binom :: Int -> Int -> Int
    binom _ 0 = 1
    binom 0 _ = 0
    binom n k = let b = binom (n - 1) (k - 1)
        in seq b n * b `div` k
    
    -- | Computes the n-th Catalan number
    catalan :: Int -> Int
    catalan n = binom (2 * n) n `div` (n + 1)

    -- | Returns whether the given list
    -- has length less or equal to k.
    -- Time: min(length [a], k)
    leq :: [a] -> Int -> Bool
    leq [] k = True
    leq _ 0 = False
    leq (x:xs) k = leq xs (k - 1)

    -- | Returns whether the given list
    -- has length equal to k. Time: min(length [a], k)
    eq :: [a] -> Int -> Bool
    eq [] 0 = True
    eq [] k = False
    eq (x:xs) k = eq xs (k - 1)
 
    -- | Returns whether the given list
    -- has length greater or equal to k.
    -- Time: min(length [a], k)
    geq :: [a] -> Int -> Bool
    geq _ 0 = True
    geq [] k = False
    geq (x:xs) k = geq xs (k - 1)
