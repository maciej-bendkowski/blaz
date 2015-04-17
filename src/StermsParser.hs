{-|
 - Module       : StermsParser
 - Description  : S-term parser
 - Copyright    : (c) Maciej Bendkowski
 -
 - Maintainer   : maciej.bendkowski@gmail.com
 - Stability    : experimental
 -}
module StermsParser where
    
    import ParserUtils
    import Sterms
    
    -- S-term grammar
    -- <combinator> := "S"
    -- <subterm> := <combinator> | "(" <term> ")"
    -- <term> := <subterm>+
    
    -- | A primitive S-term combinator parser.
    combinatorParser :: Parser Term
    combinatorParser = do
        _ <- symb "S"
        return S
    
    -- | An S-term subterm parser.
    subtermParser :: Parser Term
    subtermParser = termParser' `dmplus` combinatorParser where
        termParser' :: Parser Term
        termParser' = do
            _ <- symb "("
            t <- termParser
            _ <- symb ")"
            return t
    
    -- | An S-term term parser.
    termParser :: Parser Term
    termParser = subtermsParser where
        subtermsParser :: Parser Term
        subtermsParser = do
            ts <- pstar subtermParser
            return $ apply' (head ts) (tail ts) where
                apply' :: Term -> [Term] -> Term
                apply' t [] = t
                apply' t (t':[]) = App t t'
                apply' t (t':ts) = apply' (App t t') ts
    
    -- | Attempts to parse an S-term term
    -- from the given string.
    parseSterm :: String -> Maybe Term
    parseSterm s = case apply termParser s of
        (x:_) -> if null (snd x) then
             Just $ fst x else Nothing
        _ -> Nothing
