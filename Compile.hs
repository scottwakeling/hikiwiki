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
    compile     :: f -> FilePath -> String -> FilePath -> IO ()
    postProcess :: f -> FilePath -> String -> FilePath -> IO () 

newtype MarkdownFile = FilePathToMarkdownFile
    { markdownFileToFilePath :: FilePath
    }
    deriving (Show)

instance WikiSrc MarkdownFile where
    {- Patches 'href="stylesheets' occurrences to be relative to the output
     - file in the publish directory, e.g. 'href="../stylesheets' where the
     - output file location is one dir deep. -}
    postProcess _ html theme etcDir = do
        let ss = "stylesheets"
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
    compile f output theme etcDir = do
        let path = markdownFileToFilePath f
        putStrLn $ "Compiling " ++ path ++ " to " ++ output
        createDirectoryIfMissing True (fst (splitFileName output))
        rawSystem "pandoc" [ "-B"
                           , etcDir ++ "/themes/" ++ theme ++ "/pre.mdwn"
                           , "-A"
                           , etcDir ++ "/themes/" ++ theme ++ "/post.mdwn"
                           , path
                           , "-o"
                           , output
                           ]
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
    compile (FilePathToMarkdownFile x) (addExtension (dropExtension ("public_html/" ++ x)) "html") theme etcDir
    postProcess (FilePathToMarkdownFile x) (addExtension (dropExtension ("public_html/" ++ x)) "html") theme etcDir
    compileSrc xs theme etcDir
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




