--
---- Copyright (c) 2015 Scott Wakeling - http://www.diskfish.org/
---- GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)
--
import Data.Char (toUpper)
import Data.List.Utils
import Data.Maybe (fromMaybe)
import System.Process
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath ((</>))
import System.FilePath.Posix
import System.IO
import Text.Regex.Posix

import Wiki

{-
 - Reads a wiki name from stdin.
 -
 - TODO: Make this getWiki, and ask for default theme etc.?
 - -}
getWikiName :: IO WikiName
getWikiName = do
    putStrLn "What will the wiki be named?"
    wikiName <- getLine
    return wikiName


{-
 - Dispatch table, maps command line arguments to command processor functions.
 - -}
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("remove", removeWikiCommand) 
           , ("init", initWikiCommand)
           , ("rebuild", rebuildWikiCommand)
           , ("refresh", refreshWikiCommand)
           , ("version", versionCommand)
           ]


removeWikiCommand :: [String] -> IO ()
removeWikiCommand [wikiName] = do
    run (RemoveWiki wikiName)


rebuildWikiCommand :: [String] -> IO ()
rebuildWikiCommand [wikiName] = do
    run (RebuildWiki wikiName)


refreshWikiCommand :: [String] -> IO ()
refreshWikiCommand [wikiName] = do
    run (RefreshWiki wikiName)


initWikiCommand :: [String] -> IO ()
initWikiCommand _ = do
    wikiName <- getWikiName
    run (InitWiki wikiName)
    run (RebuildWiki wikiName)
    return ()


{-
 - The current HikiWiki version string.
 - -}
versionString = "HikiWiki v0.1.0.0"


{-
 - Command: --version
 - Prints a simple version string.
 - -}
versionCommand :: [String] -> IO ()
versionCommand [] = putStrLn versionString


showUsage :: IO ()
showUsage = do
    putStrLn $ "Usage: HikiWiki <command> <args>"
    putStrLn $ "where possible commands include:"
    putStrLn $ "  init"
    putStrLn $ "  rebuild <wiki>"
    putStrLn $ "  refresh <wiki>"
    putStrLn $ "  remove <wiki>"
    putStrLn $ "  version"
    return ()

{-
 - Main entry point.
 - Dispatches command line arguments to command functions.
 - -}
main :: IO ()
main = do
    (commandAndArgs) <- getArgs
    case commandAndArgs of
        [] -> showUsage
        (command:args) -> case lookup command dispatch of
            Just action -> action args
            Nothing -> do
                putStrLn $ "Unrecognized command: " ++ command
                return ()
    return ()

