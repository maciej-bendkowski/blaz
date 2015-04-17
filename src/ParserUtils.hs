{-# LANGUAGE DoAndIfThenElse #-}
{-|
 - Module      :  ParserUtils
 - Description :  Generic recursive descent parser utilities following the 
 -                ideas of G.Hutton - Functional Pearls: Monadic Parsing in
 -                Haskell (https://www.cs.nott.ac.uk/~gmh/pearl.pdf) 
 - Copyright   :  (c) Maciej Bendkowski
 -
 - Maintainer  :  maciej.bendkowski@gmail.com
 - Stability   :  experimental
-}
module ParserUtils (
    Parser(..), dmplus, itemPred,
    token, star, pstar, symb, apply
) where

    import Control.Applicative
    import Control.Monad
    import Data.Char
    
    -- | Recursive descent parser type definition
    newtype Parser a = Parser (String -> [(a, String)])
    parse :: Parser a -> String -> [(a, String)]
    parse (Parser p) = p
    
    instance Functor Parser where
        fmap = liftM

    instance Applicative Parser where
        pure = return
        (<*>) = ap

    instance Alternative Parser where
        (<|>) = mplus
        empty = mzero

    -- | Monadic parser definition
    instance Monad Parser where
        return x = Parser $ \s -> [(x, s)]
        p >>= f = Parser $ \s -> concat [ parse (f x) s' | (x, s') <- parse p s]
        
    instance MonadPlus Parser where
        mzero = Parser $ const []
        p `mplus` q = Parser $ \s -> parse p s ++ parse q s
        
    -- | Deterministic choice argument
    dmplus :: Parser a -> Parser a -> Parser a
    p `dmplus` q = Parser $ \s -> case parse (p `mplus` q) s of
        (x:_) -> [x]
        _ -> []
    
    -- | Single char parser
    item :: Parser Char
    item = Parser $ \s -> case s of
        x:xs -> [(x, xs)]
        _ -> []
    
    -- | Single char predicate parser
    itemPred :: (Char -> Bool) -> Parser Char
    itemPred p = do
        x <- item
        if p x then return x
        else mzero
    
    -- | Single functional char parser
    char :: Char -> Parser Char
    char x = itemPred (x ==)
    
    -- | Single functional string parser
    str :: String -> Parser String
    str "" = return ""
    str (x:xs) = do
        _ <- char x
        _ <- str xs
        return (x:xs)
        
    -- | Kleene star operator
    star :: Parser a -> Parser [a]
    star p = pstar p `dmplus` return [] 
    
    -- | Kleene plus operator
    pstar :: Parser a -> Parser [a]
    pstar p = do
        x <- p
        xs <- star p
        return (x:xs)
        
    -- | Whitespace parser
    space :: Parser String
    space = star $ itemPred isSpace' where
        isSpace' x = isSpace x && (x /= '\n')
    
    -- | Token parser removing trailing space
    token :: Parser a -> Parser a
    token p = do
        x <- p
        _ <- space
        return x
    
    -- | Symbolic token parser
    symb :: String -> Parser String
    symb s = token $ str s
    
    -- | Applies a parser removing leading whitespace
    apply :: Parser a -> String -> [(a, String)]
    apply p = parse (do { _ <- space; p })
