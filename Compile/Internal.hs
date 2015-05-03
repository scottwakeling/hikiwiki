module Compile.Internal where


import Control.Monad (forM)
import System.Directory
import System.FilePath.Posix
import Text.Regex.Posix


{-
 - Returns a recursive list of all file paths in the directory specified
 - excluding ., .., and .git.
 - TODO: Take a glob filter, so we can ask for all .mdwn, for example.
 - -}
getSrcFilesRecursive :: FilePath -> IO [FilePath]
getSrcFilesRecursive topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", "..", ".git"]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        case isDirectory of
            True    -> getSrcFilesRecursive path
            False   -> return [path]
    return (concat paths)


