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


{- Things that are compiled and processed into wikis. -}
class WikiSrc f where
    compile     :: f -> String -> FilePath -> IO ()
    postProcess :: f -> String -> FilePath -> IO ()


newtype MarkdownFile = FilePathToMarkdownFile
    { markdownFileToFilePath :: FilePath
    }
    deriving (Show)


newtype ImageFile = FilePathToImageFile
    { imageFileToFilePath :: FilePath
    }
    deriving (Show)


{- TODO: Represent steps in compile and postProcess with some IR? -}
instance WikiSrc MarkdownFile where
    {- Patches 'href="stylesheets' occurrences to be relative to the output
     - file in the publish directory, e.g. 'href="../stylesheets' where the
     - output file location is one dir deep. -}
    postProcess f theme etcDir = do
        let ss = "stylesheets"
        let path = markdownFileToFilePath f
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

    {- Compile this markdown file to html. -}
    compile f theme etcDir = do
        let path = markdownFileToFilePath f
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


instance WikiSrc ImageFile where
    postProcess _ _ _ = do
        return ()
    compile f _ _ = do
        let path = imageFileToFilePath f
        let dst = addExtension (dropExtension ("public_html/" ++ path)) "jpg"
        putStrLn $ "Copying " ++ path ++ " to " ++ dst
        createDirectoryIfMissing True (fst (splitFileName dst))
        rawSystem "cp" [path, dst]
        return ()


{-
 - Compiles the input list of source files to the default output location,
 - replacing the .mdwn extensions with .html
 - TOOD: Should not assume public_html is in .
 - -}
compileSrc :: [FilePath] -> String -> FilePath -> IO ()
compileSrc [] _ _ = do
    return ()
compileSrc (x:xs) theme etcDir = do
    case isMarkdown x of
        True -> compileMarkdownFile x theme etcDir
        False -> compileImageFile x theme etcDir
    compileSrc xs theme etcDir
    return ()
  where
    compileMarkdownFile x theme etcDir = do
      compile (FilePathToMarkdownFile x) theme etcDir
      postProcess (FilePathToMarkdownFile x) theme etcDir
      return ()
    compileImageFile x theme etcDir = do
      compile (FilePathToImageFile x) theme etcDir
      postProcess (FilePathToImageFile x) theme etcDir
      return ()

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




