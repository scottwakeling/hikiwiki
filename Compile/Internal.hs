module Compile.Internal where


import Control.Monad (forM)
import System.Directory
import System.FilePath.Posix
import Text.Regex.Posix


splitFilePath :: FilePath -> String -> (String,String,String)
splitFilePath path ext = (snd (splitFileName path) =~ ext)


isMarkdown :: FilePath -> Bool
isMarkdown path = case (splitFilePath path ".mdwn") of
    (_,".mdwn","") -> True
    _ -> False


isImage :: FilePath -> Bool
isImage path = case (splitFilePath path ".jpg") of
    (_,".jpg","") -> True
    _ -> False


{-
 - Returns a recursive list of all markdown file paths beneath 'topDir'.
 - -}
getSrcFilesRecursive :: FilePath -> IO [FilePath]
getSrcFilesRecursive topDir = do
    names <- getDirectoryContents topDir
    let properNames = filter (isSrcFileOrDir) names
    paths <- forM properNames $ \name -> do
        let path = topDir </> name
        isDirectory <- doesDirectoryExist path
        case isDirectory of
            True    -> getSrcFilesRecursive path
            False   -> return [path]
    return (concat paths)
  where
    isSrcFileOrDir :: FilePath -> Bool
    isSrcFileOrDir x = do
        case hasExtension x of
            True  -> case isMarkdown x of
                         True -> True
                         False -> isImage x
            False -> notElem x [".", "..", ".git"]


