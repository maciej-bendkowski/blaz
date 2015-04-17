{- |
 - Module      :  CFGParser
 - Description :  Context-free grammar parser
 - Copyright   :  (c) Maciej Bendkowski
 -
 - Maintainer  :  maciej.bendkowski@gmail.com
 - Stability   :  experimental
 -}
module CFGParser (parseCFG) where
    
    import Data.Char
    import ParserUtils
    import CFG

    -- CFG grammar
    -- <terminal> := {lowercase character}
    -- <non-terminal> := "{" <uppercase starting string> "}"
    -- <symbol> := <terminal> | <non-terminal>
    -- <production> := <non-terminal> "->" <symbol>+ ("|" <symbol>+)* {EOL}
    -- <grammar> := "start:" <non-terminal> {EOL} <production>+
    
    isSymbolDelimiter :: Char -> Bool
    isSymbolDelimiter x = (x == '{') || (x == '}') || (x == '|')
    
    isSymbolic :: Char -> Bool
    isSymbolic x = isPrint x && not (isSymbolDelimiter x)
    
    eolParser :: Parser Char
    eolParser = token $ itemPred ('\n' ==)
    
    terminalParser :: Parser Terminal
    terminalParser = do
        x <- token $ itemPred isSymbolic
        return $ Terminal x
    
    nonterminalParser :: Parser Nonterminal
    nonterminalParser = do
        _ <- symb "{"
        x <- token $ itemPred isUpper
        xs <- star . token $ itemPred isSymbolic
        _ <- symb "}"
        return $ Nonterminal (x:xs)
        
    symbolParser :: Parser Symbol
    symbolParser = terminalParser' `dmplus` nonterminalParser' where
        terminalParser' = do
            t <- terminalParser
            return $ CFGTerminal t
        
        nonterminalParser' = do
            nt <- nonterminalParser
            return $ CFGNonterminal nt
    
    productionParser :: Parser [Production]
    productionParser = do
        nt <- nonterminalParser
        _ <- symb "->"
        symbs <- pstar symbolParser
        psymbs <- star pipeProductionParser
        _ <- eolParser
        return $ Production { lhs = nt, rhs = symbs } : map (constr nt) psymbs where 
            pipeProductionParser = do
                _ <- symb "|"
                pstar symbolParser
                
            constr nt ss = Production { lhs = nt, rhs = ss }
    
    grammarParser :: Parser CFG
    grammarParser = do
        _ <- symb "start:"
        s <- nonterminalParser
        _ <- eolParser
        ps <- pstar productionParser
        return CFG { start = s, productions = concat ps }
    
    -- | Attempts to parse the given string
    -- as a context-free grammar
    parseCFG :: String -> Maybe CFG
    parseCFG s = case apply grammarParser s of
        (x:_) -> Just $ fst x
        _ -> Nothing
