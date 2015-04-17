{- |
 - Module      :  CFG
 - Description :  Context-free grammar utilities
 - Copyright   :  (c) Maciej Bendkowski
 -
 - Maintainer  :  maciej.bendkowski@gmail.com
 - Stability   :  experimental
-}
module CFG (
    Terminal(..), Nonterminal(..),
    Symbol(..), Production(..),
    CFG(..), symbol, toCNF
) where
    
    import Data.List
    import Data.Maybe
    import Data.Set (Set)
    import Data.Map (Map)

    import qualified Data.Set as Set
    import qualified Data.Map as Map
    import CommonUtils (leq)

    class Symbolic a where
        symbol :: a -> String
    
    -- | Terminal symbols
    newtype Terminal = Terminal Char deriving (Eq, Ord)
    instance Symbolic Terminal where
        symbol (Terminal s) = [s]
    
    instance Show Terminal where
        show = symbol
    
    -- | Non-terminal symbols
    newtype Nonterminal = Nonterminal String deriving (Eq, Ord)
    instance Symbolic Nonterminal where
        symbol (Nonterminal s) = s
    
    instance Show Nonterminal where
        show n = "{" ++ symbol n ++ "}"
    
    -- | Production symbols, i.e. either
    -- non-terminals or terminals
    data Symbol = CFGTerminal Terminal
        | CFGNonterminal Nonterminal deriving (Eq, Ord)
    
    instance Show Symbol where
        show (CFGNonterminal n) = show n
        show (CFGTerminal t) = show t
    
    instance Symbolic Symbol where
        symbol (CFGNonterminal x) = symbol x
        symbol (CFGTerminal x) = symbol x
    
    isTerminalSymbol :: Symbol -> Bool
    isTerminalSymbol (CFGTerminal _) = True
    isTerminalSymbol _ = False
    
    isNonterminalSymbol :: Symbol -> Bool
    isNonterminalSymbol = not . isTerminalSymbol

     -- | Context-free grammar production
    data Production = 
        Production { lhs :: Nonterminal, rhs :: [Symbol] }
            deriving (Eq, Ord)
        
    instance Show Production where
        show p = show' p "" where
            show' p = (show (lhs p) ++) . (" -> " ++) . symb (rhs p)
            symb = foldr (\x -> (.) (show x ++)) ("" ++)
    
    -- | Context-free grammar
    data CFG =  CFG { start :: Nonterminal, productions :: [Production] }
    
    instance Show CFG where
        show g = show' g "" where
            show' g = (("start: " ++ show (start g)) ++) . prods (productions g)
            prods = foldr (\x -> (.) (('\n' : show x) ++)) ("" ++)

    -- | Returns the number of productions
    -- in the context-free grammar
    size :: CFG -> Int
    size = length . productions
    
    -- | Returns all terminal symbols
    -- in the given context-free grammar
    terminalSymbols :: CFG -> [Terminal]
    terminalSymbols g = nub . concatMap f $ productions g where
        f prod = map (Terminal . head . symbol) $
             filter isTerminalSymbol $ rhs prod
    
    -- | Returns all non-terminal symbols
    -- in the given context-free grammar
    nonterminalSymbols :: CFG -> [Nonterminal]
    nonterminalSymbols g = nub . concatMap f $ productions g where
        f prod = lhs prod : map (Nonterminal . symbol) 
             (filter isNonterminalSymbol $ rhs prod)
             
    -- | Constructs a set of non-terminal symbols
    -- for the given context-free grammar
    initNonterminalSet :: CFG -> Set Nonterminal
    initNonterminalSet = Set.fromList . nonterminalSymbols

    -- | Returns a new fresh non-terminal symbol from
    -- the given set of non-terminals
    next :: Set Nonterminal -> (Nonterminal, Set Nonterminal)
    next s = (n, Set.insert n s) where
        n = Nonterminal $ init m ++ nextChar (last m)
        m = symbol $ Set.findMax s
        nextChar 'Z' = "ZA"
        nextChar x = [succ x]
    
    -- | Returns a map of (x, A) such that A -> x
    -- for a given context-free grammar
    mapTerminals :: CFG -> Map Terminal Nonterminal
    mapTerminals g = f (terminalSymbols g) (initNonterminalSet g) where
        f [] _ = Map.empty
        f (t:ts) s = Map.insert t n $ f ts s' where
            (n, s') = next s
        
    -- | Removes the terminal symbols from the RHS
    term :: CFG -> CFG
    term g = CFG { start = start g, productions = ps } where
        ps = map rename (productions g) ++ map mkProd (Map.assocs termMap)
        mkProd (t, n) = Production { lhs = n, rhs = [CFGTerminal t] }
        termMap = mapTerminals g
        
        rename p = Production { lhs = lhs p, rhs = map rename' $ rhs p }
        rename' (CFGTerminal t) = CFGNonterminal $ 
            fromJust $ Map.lookup t termMap
        rename' x = x
    
    -- | Returns whether the given production
    -- is consistent with the Chomsky normal
    -- form or not
    isCNFProduction :: Production -> Bool
    isCNFProduction p = case rhs p of
        [CFGNonterminal _, CFGNonterminal _] -> True
        [CFGTerminal _] -> True
        _ -> False
        
    -- | Returns whether the given context-free
    -- grammar is in Chomsky normal form or not
    inCNF :: CFG -> Bool
    inCNF g = all isCNFProduction $ productions g
    
    binExpand :: Production -> Set Nonterminal -> ([Production], Set Nonterminal)
    binExpand p nts
        | rhs p `leq` 2 = ([p], nts)
        | otherwise = (p' : ps', nts'') where
            (nt, nts') = next nts
            p' = Production { lhs = lhs p, 
                rhs = [head (rhs p), CFGNonterminal nt] }
            (ps', nts'') = binExpand Production { 
                lhs = nt, rhs = tail (rhs p) } nts'
                
    binExpandNonCNF :: [Production] -> Set Nonterminal -> [Production]
    binExpandNonCNF [] nts = []
    binExpandNonCNF (p:ps) nts = ps' ++ res where
        (ps', nts') = binExpand p nts
        res = binExpandNonCNF ps nts'

    -- | Expands productions of three or more symbols
    -- into unary-binary productions
    bin :: CFG -> CFG
    bin g = CFG { start = start g, productions = ps } where
        ps = binExpandNonCNF (productions g) nts
        nts = initNonterminalSet g
    
    renameNonterminal :: Nonterminal -> Nonterminal -> Production -> Production
    renameNonterminal nt nt' p = Production { lhs = subNT (lhs p),
         rhs = map sub $ rhs p } where
            subNT x = if x == nt then nt' else x
            sub x = case x of
                CFGNonterminal s -> CFGNonterminal (subNT s)
                _ -> x
    
    -- | Ensures the start symbol does not occur on the RHS
    startSymb :: CFG -> CFG
    startSymb g = CFG { start = st, 
        productions = sp : map (renameNonterminal st st') ps } where
            (st', _) = next $ initNonterminalSet g
            sp = Production { lhs = st, rhs = [CFGNonterminal st'] }
            ps = productions g
            st = start g
    
    isUnitProduction :: Production -> Bool
    isUnitProduction Production { rhs = [CFGNonterminal _] } = True
    isUnitProduction _ = False
    
    -- | Removes unit productions in form of A -> B
    unit :: CFG -> CFG
    unit g = unit' g Set.empty where
        unit' :: CFG -> Set Production -> CFG
        unit' g us
            | null units = g
            | otherwise = unit' CFG { start = start g, productions = prods } us' where
                (units, nonunits) = partition isUnitProduction $ productions g
                
                topUnit = head units
                rest = tail units ++ nonunits
            
                ulhs = lhs topUnit
                urhs = Nonterminal $ symbol . head . rhs $ topUnit
                
                prods = concatMap sub rest
                sub p @ (Production { lhs = s, rhs = r })
                    | s == urhs = 
                        if (p { lhs = ulhs }) `Set.member` us then [p]
                        else [p, p { lhs = ulhs }]
                    | otherwise = [p]
                    
                us' = Set.insert topUnit us 
    
    -- | Transform the given context-free grammar
    -- into an equivalent one in Chomsky normal form
    toCNF :: CFG -> CFG
    toCNF = unit . bin . term . startSymb
