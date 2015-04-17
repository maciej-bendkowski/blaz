{-|
 - Module      :  Blaz
 - Description :  Blaz - S-term normalization solver
 - Copyright   :  (c) Maciej Bendkowski
 -
 - Maintainer  :  maciej.bendkowski@gmail.com
 - Stability   :  experimental
 -}
module Main (main) where
    
    import Data.List
    import Data.Maybe (fromJust)
    import Data.Text (pack, unpack, strip)
    
    import System.IO
    import System.Exit
    import System.Console.GetOpt
    import System.Environment
    
    import StermsParser
    import Sterms
    
    data Flag = Size String | Reductions String 
        | Help | Version deriving (Eq)
    
    options :: [OptDescr Flag]
    options = [Option "r" ["reductions"] (ReqArg Reductions "n")
        "Use preprocessing by performing n reduction steps",
    
        Option "s" ["size"] (ReqArg Size "n")
            "Computes all non-normalizing S-terms of size n",
    
        Option "v" ["version"] (NoArg Version)
            "Print the program version number",

        Option "h?" ["help"] (NoArg Help) 
            "Print this help message"]
            
    usageHeader :: String
    usageHeader = "Usage: blaz [OPTION...] [FILE...]"
    
    versionHeader :: String
    versionHeader = "Blaz version 1.0, (C) Maciej Bendkowski 2015"
    
    parse :: [String] -> IO ([Flag], [String])
    parse argv = case getOpt RequireOrder options argv of
        (ops, nonops, [])
            | Help `elem` ops -> do
                putStrLn $ usageInfo usageHeader options
                exitSuccess
            | Version `elem` ops -> do
                putStrLn versionHeader
                exitSuccess
            | otherwise -> return (nub (concatMap mkset ops), fs) where
                fs = if null nonops then ["-"] else nonops
                mkset x = [x]
        (_, _, errs) -> do
            hPutStrLn stderr (concat errs ++ usageInfo usageHeader options)
            exitWith (ExitFailure 1)
            
    blaz :: (Term -> Bool) -> [String] -> [String]
    blaz f = map (show . f . fromJust . parseSterm)
    
    blaz' :: (Term -> Bool) -> [Term] -> [String]
    blaz' f ts = map show $ filter (not . f) ts

    withInput s f = putStr . unlines . f . lines =<< open s where
        open f = if f == "-" then getContents else readFile f

    run :: [Flag] -> String -> IO ()
    run fgs input
        | s >= 0 = putStr . unlines $ blaz' f (ofSize s)
        | otherwise = withInput input (blaz f) where
            f = if rs == 0 then isNormalizable else normalizable rs
            rs = getReductions fgs
            s = getSize fgs
    
    getReductions :: [Flag] -> Int
    getReductions (Reductions r : fgs) = read r
    getReductions (f : fgs) = getReductions fgs
    getReductions [] = 0
    
    getSize :: [Flag] -> Int
    getSize (Size n : fgs) = read n
    getSize (f : fgs) = getSize fgs
    getSize [] = -1

    main :: IO ()
    main = do
        (ops, fs) <- getArgs >>= parse
        mapM_ (run ops) fs
        exitSuccess
