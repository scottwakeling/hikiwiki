--
---- Copyright (c) 2015 Scott Wakeling - http://www.diskfish.org/
---- GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)
--
module Compile
    (
      compileWiki
    ) where


import Control.Monad (forM)
import Data.List.Utils
import System.Process
import System.Directory
import System.FilePath.Posix
import Text.Regex.Posix

import Compile.Internal
import Yaml

{- A class of types that can be compiled and post-processed. -}
class SourceFiles f where
    compile     :: f -> String -> FilePath -> IO ()
    postProcess :: f -> String -> FilePath -> IO ()

data SourceFile = MarkdownFile FilePath | ImageFile FilePath

getSrcFilePath :: SourceFile -> FilePath
getSrcFilePath (MarkdownFile f) = f
getSrcFilePath (ImageFile f) = f

{- TODO: Represent steps in compile and postProcess with some IR? -}
instance SourceFiles SourceFile where
    {- Compile markdown to html. -}
    compile (MarkdownFile path) theme etcDir = do
        let dst = addExtension (dropExtension ("public_html/" ++ path)) "html"
        putStrLn $ "Compiling " ++ path ++ " to " ++ dst
        createDirectoryIfMissing True (fst (splitFileName dst))
        rawSystem "pandoc" [ "-B"
                           , etcDir ++ "/themes/" ++ theme ++ "/pre.mdwn"
                           , "-A"
                           , etcDir ++ "/themes/" ++ theme ++ "/post.mdwn"
                           , path
                           , "-o"
                           , dst
                           ]
        return ()
    {- Compile images by copying them into place. -}
    compile (ImageFile path) _ _ = do
        let dst = addExtension (dropExtension ("public_html/" ++ path)) "jpg"
        putStrLn $ "Copying " ++ path ++ " to " ++ dst
        createDirectoryIfMissing True (fst (splitFileName dst))
        rawSystem "cp" [path, dst]
        return ()
    {- Patches 'href="stylesheets' occurrences to be relative to the output
     - file in the publish directory, e.g. 'href="../stylesheets' where the
     - output file location is one dir deep. -}
    postProcess (MarkdownFile path) theme etcDir = do
        let ss = "stylesheets"
        let html = addExtension (dropExtension ("public_html/" ++ path)) "html"
        replace ("href=\"" ++ ss) ("href=\"" ++ (srcRoot html) ++ ss) html
        return ()
      where
        count str c = length $ filter (== c) str
        srcRoot path = foldl (++) "" (take (count path '/' -2) $ repeat "../")
        replace string replacement output = do
            rawSystem "sed" [ "-i"
                            , "s~" ++ string ++ "~" ++ replacement ++ "~g"
                            , output]
    postProcess (ImageFile path) _ _ = do
        return ()

{-
 - Compiles an input list of SourceFiles to the default output location,
 - replacing the .mdwn extensions with .html
 - TOOD: Should not assume public_html is in .
 - -}
compileSrc :: (SourceFiles f) => [f] -> String -> FilePath -> IO ()
compileSrc [] _ _ = do
    return ()
compileSrc (x:xs) theme etcDir = do
    compile x theme etcDir
    postProcess x theme etcDir
    compileSrc xs theme etcDir
    return ()


{-
 - Returns a recursive list of all markdown file paths beneath 'root'.
 - -}
getSrcFilesRecursive :: FilePath -> IO [SourceFile]
getSrcFilesRecursive root = do
    names <- getDirectoryContents root
    let properNames = filter (isSrcFileOrDir) names
    paths <- forM properNames $ \name -> do
        let path = root </> name
        isDirectory <- doesDirectoryExist path
        case isDirectory of
            True    -> getSrcFilesRecursive path
            False   -> case isMarkdown path of
                           True  -> return [MarkdownFile path]
                           False -> return [ImageFile path]
    return (concat paths)
  where
    isSrcFileOrDir :: FilePath -> Bool
    isSrcFileOrDir x = do
        case hasExtension x of
            True  -> case isMarkdown x of
                         True -> True
                         False -> isImage x
            False -> notElem x [".", "..", ".git"]


{-
 - Compiles all input files in the source directory provided to the default
 - destination directory.
 -
 - TODO: Assumes the src dir and public_html are in .
 -       Should take a Wiki?
 -       Should only pass source files (e.g. .mdwn) to compileSrc, not
 -       everything in the location provided..
 - -}
compileWiki :: [(String,String)] -> FilePath -> IO (Bool)
compileWiki wikiConfig etcDir = do
    let wiki = lookupYaml "wikiname" wikiConfig
    case wiki of
        Nothing -> (return False)
        Just wiki -> do
            tryCompile wiki
            (return True)
  where
    tryCompile wiki = do
        src <- getSrcFilesRecursive wiki
        let theme = (themeName wikiConfig)
        compileSrc src theme etcDir
        putStrLn $ "Copying stylesheets for " ++ theme
        rawSystem "cp" [ "-r"
                       , etcDir ++ "/themes/" ++ theme ++ "/stylesheets/"
                       , "public_html/" ++ wiki
                       ]
    themeName :: [(String,String)] -> String
    themeName wikiConfig = case (lookupYaml "theme" wikiConfig) of
        Nothing -> "cayman"
        Just theme -> theme




