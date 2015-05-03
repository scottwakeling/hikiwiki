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
import System.Cmd
import System.Directory
import System.FilePath.Posix
import Text.Regex.Posix

import Compile.Internal
import Yaml

{-
 - Compiles input .mdwn file to output .html file.
 -
 - Patches 'href="stylesheets' occurrences to be relative to output file in publish
 - directory, e.g. 'href="../stylesheets' for one dir down.
 -
 - TODO: Refactor the post-processing out of here..
 - -}
compile :: FilePath -> FilePath -> String -> IO ()
compile input output theme = do
    putStrLn $ "Compiling " ++ input ++ " to " ++ output
    createDirectoryIfMissing True (fst (splitFileName output))
    rawSystem "pandoc" [ "/home/scott/hikiwiki/etc/themes/" ++ theme ++ "/pre.mdwn"
                       , input
                       , "/home/scott/hikiwiki/etc/themes/" ++ theme ++ "/post.mdwn"
                       , "-o"
                       , output
                       ]
    replaceInFile "href=\"stylesheets" ("href=\"" ++ (srcRoot output) ++ "stylesheets") output
    return ()
  where
    countLetters str c = length $ filter (== c) str
    srcRoot path = foldl (++) "" (take (countLetters path '/' -2) $ repeat "../")
    replaceInFile string replacement output = do
        rawSystem "sed" [ "-i"
                        , "s~" ++ string ++ "~" ++ replacement ++ "~g"
                        , output]


{-
 - Compiles the input list of source files to the default output location,
 - replacing the .mdwn extensions with .html
 - TOOD: Should not assume public_html is in .
 - -}
compileSrc :: [FilePath] -> String -> IO ()
compileSrc [] _ = do
    return ()
compileSrc (x:xs) theme = do
    compile x (replace ".mdwn" ".html" ("public_html/" ++ x)) theme
    compileSrc xs theme
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
compileWiki :: [(String,String)] -> IO (Bool)
compileWiki wikiConfig = do
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
        compileSrc src theme
        putStrLn $ "Copying stylesheets for " ++ theme
        rawSystem "cp" [ "-r"
                       , "etc/themes/" ++ theme ++ "/stylesheets/"
                       , "public_html/" ++ wiki
                       ]
    themeName :: [(String,String)] -> String
    themeName wikiConfig = case (lookupYaml "theme" wikiConfig) of
        Nothing -> "cayman"
        Just theme -> theme




