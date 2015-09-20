module Compile.Internal where


import Control.Monad (forM)
import System.Directory
import System.FilePath.Posix
import Text.Regex.Posix


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
    isMarkdown :: FilePath -> Bool
    isMarkdown path = case (splitSrcPath path) of
        (_,".mdwn","") -> True
        _ -> False
      where
        splitSrcPath :: FilePath -> (String,String,String)
        splitSrcPath path = (snd (splitFileName path) =~ ".mdwn")
    isSrcFileOrDir :: FilePath -> Bool
    isSrcFileOrDir x = do
        case hasExtension x of
            True  -> isMarkdown x
            False -> notElem x [".", "..", ".git"]


