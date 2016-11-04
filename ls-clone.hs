{-# OPTIONS_GHC -fno-cse #-}

import System.Console.CmdArgs
import ParseArgs
import Text.Printf (printf)
import System.Directory (getDirectoryContents, listDirectory, 
                            getCurrentDirectory)
import System.Posix.Files (isDirectory, getFileStatus)
import System.FilePath ((</>))
import Data.List (isPrefixOf, stripPrefix, sortBy)
import Data.Char (toLower)
import Prelude hiding (all)
import Control.Monad (filterM, unless)
import Data.Maybe (fromJust)

data DirInfo = DirInfo {
    dirName :: String,
    files :: [FilePath]
};

main :: IO ()
main = do
    argFlags <- cmdArgs ls
    contents <- getFiles argFlags
    let sorted = map (filterAndSort argFlags) contents
    prettyprint argFlags sorted

prettyprint :: LS -> [DirInfo] -> IO ()
prettyprint a d
    | recursive a = recursivePrint (dirSort d) a
    | otherwise = mapM_ basicPrint d

dirSort :: [DirInfo] -> [DirInfo]
dirSort = sortBy compName

--sort our directory info ignoring . and /
compName :: DirInfo -> DirInfo -> Ordering
compName a b
    | rs (dirName a) > rs (dirName b) = GT
    | otherwise = LT
    where rs = removeslash
          removeslash = filter (`notElem` ['.', '/'])

--don't print the two spaces on the final item
basicPrint :: DirInfo -> IO ()
basicPrint d = do
    mapM_ (printf "%s  ") (init (files d))
    putStr $ last (files d)
    putStr "\n"

--print the dirname unless -a / -A hasn't been set and it's a hidden folder
recursivePrint' :: DirInfo -> LS -> IO ()
recursivePrint' d a = do
    unless (noShow a (dirName d)) $ printf "%s:\n" (dirName d)
    --need to check files aren't null otherwise init/last will fail
    unless (null (files d)) $ do
        mapM_ (printf "%s  ") (init (files d))
        putStr $ last (files d)

recursivePrint :: [DirInfo] -> LS -> IO ()
recursivePrint [] _ = return ()
recursivePrint (x:[]) a = recursivePrint' x a
recursivePrint (x:xs) a = do
    recursivePrint' x a
    if (null $ files x) then putStr "\n" else putStr "\n\n"
    recursivePrint xs a

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

getFiles :: LS -> IO [DirInfo]
getFiles a
    | recursive a = do
        cwd <- getCurrentDirectory
        recurseGetFiles cwd cwd
    | otherwise = do
        d <- getDirectoryContents "."
        return [DirInfo "." d]

filterAndSort :: LS -> DirInfo -> DirInfo
filterAndSort a d = let filtered = customFilter d a
                    in  d { files = sortFunc a filtered }

recurseGetFiles :: FilePath -> FilePath -> IO [DirInfo]
recurseGetFiles cwd path = do
    contents <- listDirectory path
    let paths = map (path </>) contents
        name' = relativeDir cwd path
        contInfo = DirInfo name' contents
    dirs <- filterM (\x -> isDirectory <$> getFileStatus x) paths
    newpaths <- concat <$> mapM (recurseGetFiles cwd) dirs
    return (contInfo : newpaths)

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
    | (nm a) > (nm b) = GT
    | otherwise = LT
    where nm = map toLower . filter (/= '.')
