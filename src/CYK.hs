{- |
 - Module      :  CYK
 - Description :  CYK algorithm
 - Copyright   :  (c) Maciej Bendkowski
 -
 - Maintainer  :  maciej.bendkowski@gmail.com
 - Stability   :  experimental
 -}
module CYK where
    
    import Data.Array
    import Data.List
    import Data.Set (Set)
    import qualified Data.Set as Set

    type Symbol = Char
    type Rule = String

    -- | Chomsky normal form productions, 
    -- i.e. A -> BC or A -> x
    data Production = 
          NonTerminal Rule (Rule, Rule) -- Production: A -> BC 
        | Terminal Rule Symbol -- Production A -> b
   
    -- | Opaques the given string into a
    -- non-terminal representation
    opaque :: String -> String
    opaque s = "{" ++ s ++ "}"
        
    instance Show Production where
        show (NonTerminal s (r, l)) = opaque s ++ " -> " ++ opaque r ++ opaque l
        show (Terminal s t) = opaque s ++ " -> " ++ [t]
  
    -- | Returns the LHS of a production
    productionLHS :: Production -> Rule
    productionLHS (NonTerminal rule _) = rule
    productionLHS (Terminal rule _) = rule
   
    -- | Returns whether the given production
    -- is a terminal production or not
    isTerminal :: Production -> Bool
    isTerminal (Terminal _ _) = True
    isTerminal _ = False

    -- | Returns whether the given production
    -- is a non-terminal production
    isNonTerminal :: Production -> Bool
    isNonTerminal = not . isTerminal

    -- | Returns whether the given production
    -- generates the symbol s or not
    producesSymbol :: Symbol -> Production -> Bool
    producesSymbol s (Terminal _ t) = t == s
    producesSymbol _ _ = False
    
    -- | Returns whether the given production
    -- generates the rule pair or not
    producesRule :: (Rule,Rule) -> Production -> Bool
    producesRule (b,c) (NonTerminal _ (b',c')) = b' == b && c' == c
    producesRule _ _ = False

    type Productions = [Production]
    type AWord = Array Int Symbol
    type Rules = Set Rule
    
    -- | Context-free grammar in Chomsky normal form
    data Grammar = Grammar { 
        startRule :: Rule, 
        productions :: Productions
    }
    
    instance Show Grammar where
        show g = show' g "" where
            show' g = (("start: " ++ startRule g) ++) . prods (productions g)
            
            prods :: [Production] -> ShowS
            prods = foldr (\x -> (.) (('\n' : show x) ++)) ("" ++)
    
    -- | Returns the number of productions 
    -- in the given context-free grammar
    size :: Grammar -> Int
    size = length . productions

    type CYKMatrix = Array (Int, Int) Rules

    -- | Computes the CYK matrix for the given
    -- production list and input word of size n
    cykMatrix :: Productions -> AWord -> Int -> CYKMatrix
    cykMatrix productions word n = matrix where
        
        -- filters all terminal productions
        terminalProductions :: Productions
        terminalProductions = filter isTerminal productions
        
        -- filters all non-terminal productions
        nonTerminalProductions :: Productions
        nonTerminalProductions = filter isNonTerminal productions

        matrix = array ((1,1), (n,n)) $
            [((i,i), Set.fromList $ map productionLHS $ terminalRules (word ! i)) | i <- [1..n]] ++
            [((i,i+h-1), induction i h) | h <- [2..n], i <- [1..(n-h+1)]] where
                
                -- returns all terminals productions generating s
                terminalRules :: Symbol -> Productions
                terminalRules s = filter (producesSymbol s) terminalProductions

                -- returns all non-terminals productions generating both rules
                nonTerminalRules :: (Rule, Rule) -> Productions
                nonTerminalRules p = filter (producesRule p) nonTerminalProductions

                -- main CYK induction step
                induction :: Int -> Int -> Rules
                induction i h = Set.fromList $ concat rs where
                    rs = [map productionLHS $ nonTerminalRules (b,c) 
                        | j <- [1..h-1], b <- Set.toList (matrix ! (i,i+j-1)),
                         c <- Set.toList (matrix ! (i+j, i+h-1))]

    -- | CYK parsing algorithm returns whether
    -- the given grammar generates the input string
    cyk :: Grammar -> String -> Bool
    cyk cnfg s = start `Set.member` (matrix ! (1,n)) where
        n = length s
        w = array (1,n) $ zip [1..n] s
        matrix = cykMatrix (productions cnfg) w n
        start = startRule cnfg
