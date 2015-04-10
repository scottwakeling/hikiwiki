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


{-
 - Compiles input .mdwn file to output .html file.
 -
 - Patches 'href="stylesheets' occurrences to be relative to output file in publish
 - directory, e.g. 'href="../stylesheets' for one dir down.
 -
 - TODO: Refactor the post-processing out of here..
 - -}
compile :: FilePath -> FilePath -> IO ()
compile input output = do
    putStrLn $ "Compiling " ++ input ++ " to " ++ output
    createDirectoryIfMissing True (fst (splitFileName output))
    rawSystem "pandoc" [ "/home/scott/src/hikiwiki/etc/themes/cayman/pre.mdwn"
                       , input
                       , "/home/scott/src/hikiwiki/etc/themes/cayman/post.mdwn"
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
compileSrc :: [FilePath] -> IO ()
compileSrc [] = do
    return ()
compileSrc (x:xs) = do
    compile x (replace ".mdwn" ".html" ("public_html/" ++ x))
    compileSrc xs
    return ()


{-
 - Compiles all input files in the source directory provided to the default
 - destination directory.
 -
 - Copies the default (cayman) theme's stylesheets into the publish dir.
 -
 - TODO: Assumes the src dir and public_html are in .
 -       Should take a Wiki?
 -       Should only pass source files (e.g. .mdwn) to compileSrc, not
 -       everything in the location provided..
 -       Should read theme from the provided Wiki config, not assume cayman.
 - -}
compileWiki :: FilePath -> IO ()
compileWiki wiki = do
    src <- getSrcFilesRecursive wiki
    compileSrc src
    rawSystem "cp" [ "-r"
                   , "etc/themes/cayman/stylesheets/"
                   , "public_html/" ++ wiki
                   ]
    return ()




