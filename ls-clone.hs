import System.Console.CmdArgs (cmdArgs)
import ParseArgs (LS, ls, recursive, almost_all, all', file)
import System.Directory (getDirectoryContents, listDirectory, 
                         getCurrentDirectory, doesFileExist,
                         doesDirectoryExist)
import System.Posix.Files (isDirectory, getFileStatus)
import System.FilePath ((</>), takeDirectory, hasDrive)
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
    case null $ file argFlags of
        True -> getAndPrint argFlags =<< getCurrentDirectory
        False -> do
            {-
            have to somehow get the amount of paths, and if it's more than
            one, print empty new line if empty dir, else print contents.
            Can't do inside getAndPrint because we map over it. Can't do
            outside cause can't tell if any elements or not. Might need to 
            semi replicate getAndPrint.
            -}
            info <- mapM getCmdDir (file argFlags)
            paths <- filterMaybe <$> mapM (`handleCmdDir` argFlags) info
            mapM_ (getAndPrint argFlags) paths

filterMaybe :: [Maybe a] -> [a]
filterMaybe [] = []
filterMaybe (Nothing:xs) = filterMaybe xs
filterMaybe (Just x:xs) = x : filterMaybe xs
   
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
    | otherwise = if hasDrive fn then return (Just fn) else do
        cwd <- getCurrentDirectory
        return . Just $ cwd </> fn
    where fn = path' p
          dir = takeDirectory fn

getFiles :: LS -> FilePath -> IO [DirInfo]
getFiles a dir
    | recursive a = recurseGetFiles dir dir keepHidden
    | otherwise = do
        d <- getDirectoryContents dir
        return [DirInfo dir d]
    where keepHidden = almost_all a || all' a

recurseGetFiles :: FilePath -> FilePath -> Bool -> IO [DirInfo]
recurseGetFiles cwd path keepHidden = do
    contents <- removeHidden keepHidden <$> listDirectory path
    let paths = map (path </>) contents
        name' = relativeDir cwd path
        --add "." and ".." here so we don't recurse forever
        contInfo = DirInfo name' ("." : ".." : contents)
    dirs <- filterM (\x -> isDirectory <$> getFileStatus x) paths
    newpaths <- concat <$> mapM (\x -> recurseGetFiles cwd x keepHidden) dirs
    return $ contInfo : newpaths

--case expression is much clearer than if here
{-# ANN module "HLint: ignore Use if" #-}
getCmdDir :: FilePath -> IO PathInfo
getCmdDir path = do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    let exists = isFile || isDir
    case exists of
        False -> fail'
        True -> if isDir then dir else item
    where fail' = return $ PathInfo path False False
          item = return $ PathInfo path False True
          dir = return $ PathInfo path True True
