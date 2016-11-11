import System.Console.CmdArgs (cmdArgs)
import ParseArgs (LS, ls, recursive, almost_all, all', file)
import System.Directory (getDirectoryContents, listDirectory, 
                         getCurrentDirectory, doesFileExist,
                         doesDirectoryExist)
import System.Posix.Files (isDirectory, getFileStatus)
import System.FilePath ((</>), takeDirectory)
import Control.Monad (filterM)
import System.Console.ANSI (SGR (Reset), setSGR)
import Utilities
import Printing (prettyprint, basicPrint)

data PathInfo = PathInfo {
    path' :: FilePath,
    isDir' :: Bool,
    exists' :: Bool
};

main :: IO ()
main = do
    argFlags <- cmdArgs ls
    case (null $ file argFlags) of
        True -> getAndPrint argFlags =<< getCurrentDirectory
        False -> do
            info <- getCmdDir argFlags
            mapM_ (getAndPrint argFlags) =<< handleCmdDir info argFlags
   
getAndPrint :: LS -> FilePath -> IO ()
getAndPrint a cwd = do
    contents <- filter (shouldKeep a) <$> getFiles a cwd
    let sorted = map (filterAndSort a) contents
    prettyprint a sorted
    runIfTTY $ setSGR [Reset]

handleCmdDir :: PathInfo -> LS -> IO (Maybe FilePath)
handleCmdDir p a
    | not $ exists' p = putStrLn (noExist fn) >> return Nothing
    | not $ isDir' p = basicPrint (DirInfo dir [fn]) a >> return Nothing
    | otherwise = return (Just fn)
    where dirInfo = DirInfo 
          fn = path' p
          dir = takeDirectory fn

getFiles :: LS -> FilePath -> IO [DirInfo]
getFiles a dir
    | recursive a = recurseGetFiles dir dir keepHidden
    | otherwise = do
        d <- getDirectoryContents dir
        return [DirInfo "." d]
    where keepHidden = almost_all a || all' a

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

--case expression is much clearer than if here
{-# ANN module "HLint: ignore Use if" #-}
getCmdDir :: LS -> IO PathInfo
getCmdDir a = do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    let exists = isFile || isDir
    case exists of
        False -> fail'
        True -> if isDir then dir else item
    where path = file a
          fail' = return $ PathInfo path False False
          item = return $ PathInfo path False True
          dir = return $ PathInfo path True True
