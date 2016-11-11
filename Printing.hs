module Printing where

import Utilities (DirInfo, dirSort, files, dirName, noShow, runIfTTY)
import ParseArgs (LS, recursive, nocolor)
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), 
                                ColorIntensity(..), Color(..),
                                ConsoleIntensity(..))
import System.Directory (isSymbolicLink)
import System.FilePath.Posix ((</>))
import Text.Printf (printf)
import Control.Monad (when, unless)
import System.Posix.Files (getFileStatus, isDirectory, fileAccess, 
                            isNamedPipe, isSocket, isCharacterDevice,
                            isBlockDevice)

prettyprint :: LS -> [DirInfo] -> IO ()
prettyprint a d
    | recursive a = recursivePrint (dirSort d) a
    | otherwise = mapM_ (\x -> basicPrint x a) d

--don't print the two spaces on the final item
basicPrint :: DirInfo -> LS -> IO ()
basicPrint d a
    | null f = return ()
    | length f == 1 = def (head f)
    | otherwise = do
    mapM_ (printFile a "%s  " name') (init f)
    def (last f)
    where name' = dirName d
          def = printFile a "%s\n" name'
          f = files d

{-
If we're not connected to a terminal, then we don't do any colouring,
otherwise we get weird escape characters in our output
-}
printFile :: LS -> String -> FilePath -> FilePath -> IO ()
printFile a formatter folder f = do
    runIfTTY $ setSGR [Reset] >> setColours path a
    printf formatter f
    runIfTTY $ setSGR [Reset]
    where path = folder </> f

setColours :: FilePath -> LS -> IO ()
setColours path a
    | nocolor a = return ()
    | otherwise = do
    info <- getFileStatus path
    isSym <- isSymbolicLink path
    isExec <- fileAccess path False False True
    let isDir = isDirectory info
        isPipe = isNamedPipe info
        isSock = isSocket info
        isDev = isCharacterDevice info
        isBlock = isBlockDevice info
        typeColor = [(isExec, execColor), (isDir, dirColor), (isSym, symColor),
                     (isSock, sockColor), (isDev, devColor), 
                     (isBlock, blockColor)]
    mapM_ set typeColor
    when (isPipe) $ setSGR pipeColor
    where set (x,y) = when (x) $ mapM_ setSGR [y, boldness]

dirColor = [SetColor Foreground Vivid Blue]
symColor = [SetColor Foreground Vivid Cyan]
execColor = [SetColor Foreground Vivid Yellow]
pipeColor = [SetColor Foreground Dull Green]
boldness = [SetConsoleIntensity BoldIntensity]
sockColor = [SetColor Foreground Dull Magenta]
devColor = execColor
blockColor = execColor

--print the dirname unless -a / -A hasn't been set and it's a hidden folder
recursivePrint' :: DirInfo -> LS -> Bool -> IO ()
recursivePrint' d a final = do
    --we need to reset here so the dirname doesn't get coloured
    runIfTTY $ setSGR [Reset]
    unless (noShow a (dirName d)) $ printf "%s:\n" (dirName d)
    --need to check files aren't null otherwise init/last will fail
    unless (null (files d)) $ do
        mapM_ (printFile a "%s  " name') (init (files d))    
        printFile a "%s" name' (last $ files d)
        when final $ putStr "\n"
    where name' = dirName d

recursivePrint :: [DirInfo] -> LS -> IO ()
recursivePrint [] _ = return ()
recursivePrint [x] a = recursivePrint' x a True
recursivePrint (x:xs) a = do
    recursivePrint' x a False
    if null $ files x then putStr "\n" else putStr "\n\n"
    recursivePrint xs a
