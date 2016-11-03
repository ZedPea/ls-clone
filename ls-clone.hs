{-# OPTIONS_GHC -fno-cse #-}

import System.Console.CmdArgs
import ParseArgs
import Text.Printf (printf)
import System.Directory (getDirectoryContents)
import Data.List (isPrefixOf, sort)
import Data.Char (toLower)

main :: IO ()
main = do
    argFlags <- cmdArgs ls
    unfilteredContents <- getDirectoryContents "."
    let filteredContents = filter filterFunction unfilteredContents
        sortedContents = sortFunction filteredContents
    mapM_ (printf "%s  ") sortedContents
    putStrLn ""

customQS :: [String] -> [String]
customQS [] = []
customQS (x:xs) = customQS lesser ++ [x] ++ customQS greater
    where lesser = filter (\y -> noCase y < noCase x) xs
          greater = filter (\y -> noCase y >= noCase x) xs
          noCase = map toLower

--will later expand this to add different sort functions
sortFunction :: [String] -> [String]
sortFunction = customQS

--will later exapand this to add different filter functions
filterFunction :: String -> Bool
filterFunction x = not ("." `isPrefixOf` x)
