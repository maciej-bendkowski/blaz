{-|
 - Module      :  CFG conventer
 - Description :  Context-free grammar converter
 - Copyright   :  (c) Maciej Bendkowski
 -
 - Maintainer  :  maciej.bendkowski@gmail.com
 - Stability   :  experimental
 -}
module CFGConverter (convert) where
    
    import Data.Maybe
    
    import CFG (CFG)
    import CYK (Grammar)
    import qualified CFG
    import qualified CYK
    
    convert' :: CFG.Production -> Maybe CYK.Production
    convert' CFG.Production { CFG.lhs = nt, CFG.rhs = rhs } =
        case rhs of
            [CFG.CFGNonterminal t, CFG.CFGNonterminal t'] ->
                 Just $ CYK.NonTerminal (CFG.symbol nt) (CFG.symbol t, CFG.symbol t')
            [CFG.CFGTerminal t] ->
                 Just $ CYK.Terminal (CFG.symbol nt) (head $ CFG.symbol t)
            _ -> Nothing
    
    -- | Converts the given context-free grammar in Chomsky
    -- normal form to another equivalent representation
    -- better suited for the CYK algorithm
    convert :: CFG -> Maybe Grammar
    convert cfg
        | any isNothing ps = Nothing
        | otherwise = Just CYK.Grammar { CYK.startRule = s,
             CYK.productions = map fromJust ps } where
                ps = map convert' (CFG.productions cfg)
                s = CFG.symbol $ CFG.start cfg
