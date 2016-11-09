module Utilities where

import ParseArgs
import Prelude hiding (all)
import Data.List (sortBy, isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)
import Data.Char (toLower)

data DirInfo = DirInfo {
    dirName :: String,
    files :: [FilePath]
};


shouldKeep :: LS -> DirInfo -> Bool
shouldKeep a d = not $ noShow a (dirName d)

dirSort :: [DirInfo] -> [DirInfo]
dirSort = sortBy compName

--sort our directory info ignoring . and /
compName :: DirInfo -> DirInfo -> Ordering
compName a b
    | rs (dirName a) > rs (dirName b) = GT
    | otherwise = LT
    where rs = removeslash
          removeslash = filter (`notElem` ['.', '/'])

--will later expand this to add different sort functions
sortFunc :: LS -> [FilePath] -> [FilePath]
sortFunc _ = sortBy compPath

--will later expand this to add different filter functions
filtFunc :: LS -> FilePath -> Bool
filtFunc a x
    | all a = True
    | almost_all a = x `notElem` [".", ".."]
    | otherwise = not ("." `isPrefixOf` x)

customFilter :: DirInfo -> LS -> [FilePath]
customFilter d a
    | noShow a (dirName d) = []
    | otherwise = filter (filtFunc a) (files d)

{-
if hidden folder, will be formatted like so: './.hiddenFolder' - so check
third character is not '.'
If we are in -a / -A mode then we show it regardless
-}
noShow :: LS -> String -> Bool
noShow a xs
    | all a || almost_all a = False
    | otherwise = (length xs >= 3) && (xs !! 2 == '.')

filterAndSort :: LS -> DirInfo -> DirInfo
filterAndSort a d = let filtered = customFilter d a
                    in  d { files = sortFunc a filtered }

removeHidden :: Bool -> [FilePath] -> [FilePath]
removeHidden keepHidden f
    | keepHidden = f
    | otherwise = filter (\x -> not $ "." `isPrefixOf` x) f

{-
this gets the directory name which we print out before the contents
while in recursive mode
-}
relativeDir :: FilePath -> FilePath -> FilePath
relativeDir cwd path = "." ++ p
    where p = fromJust $ stripPrefix cwd path

--order filenames ignoring . and case
compPath :: FilePath -> FilePath -> Ordering
compPath a b
    | nm a > nm b = GT
    | otherwise = LT
    where nm = map toLower . filter (/= '.')
