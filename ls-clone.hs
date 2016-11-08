{-# OPTIONS_GHC -fno-cse #-}

import System.Console.CmdArgs
import ParseArgs
import Text.Printf (printf)
import System.Directory (getDirectoryContents, listDirectory, 
                            getCurrentDirectory, isSymbolicLink)
import System.Posix.Files (isDirectory, getFileStatus, getSymbolicLinkStatus)
import System.FilePath ((</>))
import Data.List (isPrefixOf, stripPrefix, sortBy)
import Data.Char (toLower)
import Prelude hiding (all)
import Control.Monad (filterM, unless, when)
import Data.Maybe (fromJust)
import System.Console.ANSI

data DirInfo = DirInfo {
    dirName :: String,
    files :: [FilePath]
};

main :: IO ()
main = do
    argFlags <- cmdArgs ls
    contents <- filter (shouldKeep argFlags) <$> getFiles argFlags
    let sorted = map (filterAndSort argFlags) contents
    prettyprint argFlags sorted
    setSGR [Reset]

prettyprint :: LS -> [DirInfo] -> IO ()
prettyprint a d
    | recursive a = recursivePrint (dirSort d) a
    | otherwise = mapM_ basicPrint d

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

--don't print the two spaces on the final item
basicPrint :: DirInfo -> IO ()
basicPrint d = do
    mapM_ (printFile "%s  " name') (init (files d))
    printFile "%s\n" name' (last $ files d)
    where name' = dirName d

printFile :: String -> FilePath -> FilePath -> IO ()
printFile formatter folder f = do
    setSGR [Reset]
    info <- getFileStatus path
    let isDir = isDirectory info
    isSym <- isSymbolicLink path
    when (isDir) $ mapM_ setSGR [dirColor, boldness]
    when (isSym) $ mapM_ setSGR [symColor, boldness]
    printf formatter f
    where dirColor = [SetColor Foreground Vivid Blue]
          boldness = [SetConsoleIntensity BoldIntensity]
          symColor = [SetColor Foreground Vivid Cyan]
          path = folder </> f

--print the dirname unless -a / -A hasn't been set and it's a hidden folder
recursivePrint' :: DirInfo -> LS -> Bool -> IO ()
recursivePrint' d a final = do
    setSGR [Reset]
    unless (noShow a (dirName d)) $ printf "%s:\n" (dirName d)
    --need to check files aren't null otherwise init/last will fail
    unless (null (files d)) $ do
        mapM_ (printFile "%s  " name') (init (files d))    
        printFile "%s" name' (last $ files d)
        when final $ putStr "\n"
    where name' = dirName d

recursivePrint :: [DirInfo] -> LS -> IO ()
recursivePrint [] _ = return ()
recursivePrint [x] a = recursivePrint' x a True
recursivePrint (x:xs) a = do
    recursivePrint' x a False
    if null $ files x then putStr "\n" else putStr "\n\n"
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
        recurseGetFiles cwd cwd keepHidden
    | otherwise = do
        d <- getDirectoryContents "." 
        return [DirInfo "." d]
    where keepHidden = almost_all a || all a

filterAndSort :: LS -> DirInfo -> DirInfo
filterAndSort a d = let filtered = customFilter d a
                    in  d { files = sortFunc a filtered }

recurseGetFiles :: FilePath -> FilePath -> Bool -> IO [DirInfo]
recurseGetFiles cwd path keepHidden = do
    contents <- removeHidden (keepHidden) <$> listDirectory path
    let paths = map (path </>) contents
        name' = relativeDir cwd path
        --add "." and ".." here so we don't recurse forever
        contInfo = DirInfo name' ("." : ".." : contents)
    dirs <- filterM (\x -> isDirectory <$> getFileStatus x) paths
    newpaths <- concat <$> mapM (\x -> recurseGetFiles cwd x keepHidden) dirs
    return (contInfo : newpaths)

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
