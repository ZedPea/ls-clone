module Printing where

import Utilities (DirInfo, dirSort, files, dirName, noShow)
import ParseArgs (LS, recursive)
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), 
                                ColorIntensity(..), Color(..),
                                ConsoleIntensity(..))
import System.Directory (isSymbolicLink)
import System.FilePath.Posix ((</>))
import Text.Printf (printf)
import Control.Monad (when, unless)
import System.Posix.Files (getFileStatus, isDirectory, fileAccess)

prettyprint :: LS -> [DirInfo] -> IO ()
prettyprint a d
    | recursive a = recursivePrint (dirSort d) a
    | otherwise = mapM_ basicPrint d

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
    isExec <- fileAccess path False False True
    when (isExec) $ mapM_ setSGR [execColor, boldness]
    when (isDir) $ mapM_ setSGR [dirColor, boldness]
    when (isSym) $ mapM_ setSGR [symColor, boldness]
    printf formatter f
    where dirColor = [SetColor Foreground Vivid Blue]
          symColor = [SetColor Foreground Vivid Cyan]
          execColor = [SetColor Foreground Vivid Yellow]
          boldness = [SetConsoleIntensity BoldIntensity]
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
