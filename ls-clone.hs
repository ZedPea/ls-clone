import System.Console.CmdArgs (cmdArgs)
import ParseArgs (LS, ls, recursive, almost_all, all)
import Text.Printf (printf)
import System.Directory (getDirectoryContents, listDirectory, 
                            getCurrentDirectory)
import System.Posix.Files (isDirectory, getFileStatus)
import System.FilePath ((</>))
import Prelude hiding (all)
import Control.Monad (filterM)
import System.Console.ANSI (SGR (Reset), setSGR)
import Utilities
import Printing (prettyprint)

main :: IO ()
main = do
    argFlags <- cmdArgs ls
    contents <- filter (shouldKeep argFlags) <$> getFiles argFlags
    let sorted = map (filterAndSort argFlags) contents
    prettyprint argFlags sorted
    setSGR [Reset]

getFiles :: LS -> IO [DirInfo]
getFiles a
    | recursive a = do
        cwd <- getCurrentDirectory
        recurseGetFiles cwd cwd keepHidden
    | otherwise = do
        d <- getDirectoryContents "." 
        return [DirInfo "." d]
    where keepHidden = almost_all a || all a

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
