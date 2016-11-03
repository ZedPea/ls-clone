{-# OPTIONS_GHC -fno-cse #-}

import System.Console.CmdArgs
import ParseArgs
import Text.Printf (printf)
import System.Directory (getDirectoryContents)
import Data.List (isPrefixOf)
import Data.Char (toLower)
import Prelude hiding (all)

main :: IO ()
main = do
    argFlags <- cmdArgs ls
    conts <- getDirectoryContents "."
    let filtered = filter (filtFunc argFlags) conts
        sorted = sortFunc argFlags filtered
    mapM_ (printf "%s  ") sorted
    putStrLn ""

customQS :: [String] -> [String]
customQS [] = []
customQS (x:xs) = customQS lesser ++ [x] ++ customQS greater
    where lesser = filter (\y -> noCase y < noCase x) xs
          greater = filter (\y -> noCase y >= noCase x) xs
          noCase = map toLower

--will later expand this to add different sort functions
sortFunc :: LS -> [String] -> [String]
sortFunc a = customQS

--will later exapand this to add different filter functions
filtFunc :: LS -> String -> Bool
filtFunc a x
    | all a = True
    | almost_all a = x `notElem` [".", ".."]
    | otherwise = not ("." `isPrefixOf` x)
