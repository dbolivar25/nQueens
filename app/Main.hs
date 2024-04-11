module Main (main) where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [nStr] -> do
            let n = read nStr
            let grid = replicate n $ replicate n Empty
            let solution = solve grid
            case solution of
                [] -> putStrLn "\nNo solutions"
                _ -> do
                    putStrLn ""
                    mapM_ (putStrLn . unlines . map show) solution
                    print (length solution)
        _ -> putStrLn "Usage: queens <n>"
