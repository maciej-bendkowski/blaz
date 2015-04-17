{-|
 - Module       : Sterms
 - Description  : S-term utilities
 - Copyright    : (c) Maciej Bendkowski
 -
 - Maintainer   : maciej.bendkowski@gmail.com
 - Stability    : experimental
 -}
module Sterms where
    
    import Data.Maybe
    import CommonUtils (catalan)
    import CYK (Grammar, cyk)
    import CFGConverter
    import CFGParser
    import Memo
    import CFG

    -- | Moses Schoenfinkel's SK combinatory logic
    -- limited to one primitive combinator S
    data Term =  S | App Term Term deriving (Eq)
    
    -- | String representation following common term notations, i.e.
    -- | outermost parentheses are omitted, application is binding to the left
    instance Show Term where
        show t = show' t "" where
            show' :: Term -> ShowS
            show' S = ("S" ++)
            show' (App x  p @ (App _ _)) = show' x . ("(" ++) . show' p . (")" ++)
            show' (App x y) = show' x . show' y
    
    -- | Infinite binary tree of combinatorial classes, i.e.
    -- lists of S-terms grouped by size.
    terms :: Tree [Term]
    terms = fmap (terms' g) nats where
        g :: Int -> [Term]
        g = idx terms
        
        terms' :: (Int -> [Term]) -> Int -> [Term]
        terms' _ 0 = [S]
        terms' f n = concatMap f' $ partitions (n-1) where
            f' :: (Int, Int) -> [Term]
            f' (p, p') = [App t t' | t <- f p, t' <- f p']
            
            partitions :: Int -> [(Int, Int)]
            partitions k = [(x, y) | x <- [0..k], y <- [0..k], x + y == k]
    
    -- | Class type enforcing common combinatorial class
    -- function, e.g. object size, relative density, etc.
    class Combinatorial a where
        density :: (a -> Bool) -> Int -> (Int, Int)
        ofSize :: Int -> [a]
        size :: a -> Int
        
    instance Combinatorial Term where
        
        -- | Returns a list of S-terms of given size
        ofSize = idx terms

        -- | Returns the size of a given S-term
        size (App x y) = size x + size y + 1
        size _ = 0
       
        -- | Computes the relative density of S-terms satisfying
        -- the predicate p in the set of S-terms of size n
        density p n = (length . filter p $ ofSize n, catalan n)
   
    -- | Performs a single reduction step
    -- to the given term if possible. Returns
    -- a pair (reduct, performed reduction?)
    reduct :: Term -> (Term, Bool)
    reduct (App (App (App S x) y) z) = (App (App x z) (App y z), True)
    reduct t' = (t', False)
    
    -- | Performs a single head position reduction step
    -- to the given term if possible. Returns
    -- (reduct, redex term, performed reduction?)
    headReduction :: Term -> (Term, Term, Bool)
    headReduction t @ (App x y) = case reduct t of
        (r, True) -> (r, t, True)
        (_, False) -> case headReduction x of
            (r, _, True) -> (App r y, x, True)
            (_, _, False) -> case headReduction y of
                (r, _, True) -> (App x r, y, True)
                (_, _, False) -> (t, t, False)
    headReduction x = (x, x, False)
    
    -- | Finds the normal form a given term if possible.
    -- Note that this function may not terminate.
    normalForm :: Term -> Term
    normalForm t = case headReduction t of
        (r, _, True) -> normalForm r
        (_, _, False) -> t
        
    -- | Returns a (possibly infinite) list of intermediate
    -- reduction terms resulting from applying the head 
    -- position reduction.
    normalFormReductionList :: Term -> [Term]
    normalFormReductionList t = case headReduction t of
        (r, _, True) -> r : normalFormReductionList r
        (_, _, False) -> [t]
    
    -- | Returns a list of the first k intermediate
    -- reduction terms resulting from applying the head 
    -- position reduction.
    boundNormalForm :: Int -> Term -> [Term]
    boundNormalForm k t = take k $ normalFormReductionList t
    
    -- | Checks if the given term is a
    -- primitive combinator or not.
    isCombinator :: Term -> Bool
    isCombinator S = True
    isCombinator _ = False
    
    -- | Checks if e is a subterm of t.
    isSubterm :: Term -> Term -> Bool
    isSubterm e t @ (App t' t'')
        | e == t = True 
        | otherwise = left || right where
            left = e `isSubterm` t'
            right = e `isSubterm` t''
    isSubterm e t = e == t
    
    -- | Checks if the given term has a redex.
    hasRedex :: Term -> Bool
    hasRedex S = False
    hasRedex (App (App (App S _) _) _) = True
    hasRedex (App t' t'') = hasRedex t' || hasRedex t''
    
    -- | Checks if the given term is in normal form.
    inNormalForm :: Term -> Bool
    inNormalForm t = not $ hasRedex t
    
    -- | Checks if t reduces to r in finite reduction steps.
    -- Note that this function may not terminate.
    reducesTo :: Term -> Term -> Bool
    reducesTo t r = r `elem` normalFormReductionList t
    
    -- | Context-free grammar string representing
    -- S-terms with normalization property
    grammarString :: String
    grammarString = unlines ["start: {N}",
        "{N} -> S | {L0}S | {L0}({N}) | {H0}S | {H0}({L0}) |"
        ++ " {L1}S | {L1}({H0}) | {L2}({L1}) | {N-S}S | {L4}(SS)",
        "{H0} -> S | SS | S({N}) | SSS | SS({H0}) |"
        ++ " {B}S | {B}({H0}) | {H0-S}S | {H0-SS}(SS)",
        "{H0-S} -> {H0-SS} | {K3}", "{H0-SS} -> {L0-S} | {L1-S}",
        "{L0-S} -> {K0} | {K1}", "{L1-S} -> {B} | S({B}) | SS({L1-S})",
        "{K3} -> {K2}S | SS({K3})", "{K2} -> {K1} | SS | S({L0-S}) | SS({K2})",
        "{K1} -> {K0}S | SS({K1})", "{K0} -> S | {K1}S | SSS | SS({K0})",
        "{B} -> S(SS)", "{L0} -> S | SS | S({N}) | {L0-S}S | SSS | SS({L0})",
        "{L1} -> {L1-S}S | SS({L1})", "{L2} -> {B}(SS) | SSS(SS) |"
        ++ " {B}({B}) | {L2-S}S | SS({L2})", "{L2-S} -> {K4}S | SS({L2-S})",
        "{K4} -> {B} | SS({K4})", "{L4} -> SS({L4}) | {B}({L4}) | {L4-S}S | {L4-SS}(SS)",
        "{L4-S} -> {L4-SS} | {K6}", "{L4-SS} -> {K0}S | SS | S({H0-SS}) | SS({L4-SS})",
        "{K6} -> {K5}S | SS({K6})", "{K5} -> {K0}S | SS | S({K0}) | SS({K5})",
        "{N-S} -> S | SS | S({N}) | S({H0-S})S | SS({L0-S}) | S({H0-S})({L0-S}) |"
        ++ " S({L1-S})S | S({L1-S})({H0-S}) | S({L2-S})({L1-S}) | S({L4-S})S |"
        ++ " {K0}S | {K0}({L0-S}) | {I1}({B}) | {I2}S | {I2}({L0-S}) | {I3}S |"
        ++ " {I4}({L1-S}) | {L1}S | {I5}(SS) | {I6}S | {I6}({L0-S}) | {I7}S |"
        ++ " {I9}S | {I10}S | {L2-S}(SS) | SSS | S({L0-S})S | SS({N-S}) | S({L0-S})({N-S})",
        "{I1} -> SSS", "{I2} -> {K0}S | SS | S({H0-S}) | SS({I2})",
        "{I3} -> {K0}S | S({L4-S}) | SS({I3})", "{I4} -> SS(SS) | {I1}S",
        "{I5} -> S({B})(SS) | SS({I5})", "{I6} -> {B}(SS) | SS({I6})",
        "{I7} -> {K4}S | {B}S | {B}({L0-S}) | SS({I7})", "{I8} -> SS | S({L0-S}) | SS({I8})",
        "{I9} -> {I8}S | SS({I9})", "{I10} -> SS({I10}) | SSS | S({L0-S})S |"
        ++ " SS({L0-S}) | S({L0-S})({L0-S}) | SS(SS)S | SS(SS)({L0-S}) |"
        ++ " SSSSS | SSSS({L0-S}) | {B}(SS)(SS) | {I11}S", "{I11} -> SS({I11}) |"
        ++ " S({K4}) | S(SSS)S | S(SSS)({L0-S}) | SS(SS)(SSS) | SSSS(SSS) | {I12}S",
        "{I12} -> SS({I12}) | SS(S(SSS)) | S(SSS)(SS)"]
    
    -- | Context-free grammar representing
    -- S-terms with normalization property
    grammar :: Grammar
    grammar = fromJust . convert . toCNF $ 
        fromJust (parseCFG grammarString)

    -- | Checks whether the given S-term
    -- is normalizable or not
    isNormalizable :: Term -> Bool
    isNormalizable = cyk grammar . show

    -- | Checks whether the given S-term
    -- is normalizable or not using a
    -- reduction pre-processing
    normalizable :: Int -> Term -> Bool
    normalizable n t
        | inNormalForm (last $ boundNormalForm n t) = True
        | otherwise = isNormalizable t
