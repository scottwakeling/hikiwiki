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

{- Repo
 - -}
newtype IsBare = IsBare Bool
newtype IsShared = IsShared Bool

data Repo = LocalRepo
    { getRepoLocation :: FilePath
    , isBare :: IsBare
    , isShared :: IsShared
    }

{-
 - Splits the provided FilePath into the before string, repo name, and .git
 - E.g. "/home/scott/hikiwiki.git" -> ("/home/scott/","hikiwiki",".git")
 -}
splitRepoPath :: FilePath -> (String,String,String)
splitRepoPath path = (snd (splitFileName path) =~ ".git")


getRepoName :: Repo -> String
getRepoName repo = repoName (splitRepoPath (getRepoLocation repo))
    where repoName (s,_,_) = s


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


{- Creates a bare shared repo at the location specified. -}
initBareSharedRepo :: FilePath -> IO Repo
initBareSharedRepo repoLocation = do
    rawSystem "git" [ "init"
                    , repoLocation
                    , "--bare"
                    , "--shared"
                    ]
    return (LocalRepo repoLocation (IsBare True) (IsShared True))


{-
 - Creates and chmod ugo+x a post-update hook shell script in the Repo
 - provided. Note that git commands use GIT_DIR instead of PWD, so we need to
 - unset that to have 'cd' commands take effect and git pull to work etc.
 -
 - TODO: Assumes wiki is .. and the src dir is named after the repo sans .git.
 - -}
installPostUpdateHook :: Repo -> IO ()
installPostUpdateHook repo = do
    putStrLn "Installing post-update hook.."
    let updateHookPath = (getRepoLocation repo ++ "/hooks/post-update")
    fd <- openFile updateHookPath WriteMode
    hPutStrLn fd "#!/bin/sh\n"
    hPutStrLn fd "unset GIT_DIR"
    hPutStrLn fd ("cd ../" ++ getRepoName repo)
    hPutStrLn fd "git pull"
    hPutStrLn fd "cd .."
    hPutStrLn fd ("./HikiWiki --rebuild " ++ getRepoName repo)
    hClose fd
    perms <- getPermissions updateHookPath
    setPermissions updateHookPath (perms {executable = True})


{-
 - Creates a $WIKI-setup.yaml file containing the key,value pairs provided.
 - -}
createSetupFileFor :: WikiName -> WikiConfig -> IO FilePath
createSetupFileFor wikiName settings = do
    putStrLn "Creating setup yaml.."
    fd <- openFile configFile WriteMode
    hPutStrLn fd "# HikiWiki - YAML formatted config file\n"
    mapM (writeSetupFileLine fd) settings
    hClose fd
    canonicalizePath configFile
  where
    writeSetupFileLine fd setting = do
        hPutStrLn fd (fst setting ++ ": " ++ snd setting)
    configFile = wikiName ++ "-config.yaml"


{-
 - Creates a dest dir in public_html.
 - TODO: Assumes public_html is in . Should read it from config, or be passed
 -       the 'publish dir' or something?
 - -}
createDestDir :: WikiName -> IO FilePath
createDestDir wikiName = do
    let destDir = "public_html/" ++ wikiName
    createDirectoryIfMissing True destDir
    canonicalizePath destDir


{-
 - Creates a src dir by cloning a base repo in the current directory.
 - TODO: Assumes repo is in . and named after WikiName.
 -       Should take a Repo/RepoLocation and WikiName?
 -       Or, gets a wiki config and src dir and repo loc is in there..?
 - -}
createSrcDir :: WikiName -> FilePath -> IO FilePath
createSrcDir wikiName etc = do
    putStrLn "Creating src dir.."
    rawSystem "git" ["clone", (wikiName ++ ".git"), wikiName]
    putStrLn "Copying base template to src dir.."
    rawSystem "cp" ["-r",  (etc ++ "/."), wikiName]
    setCurrentDirectory wikiName
    rawSystem "git" ["add", "--all"]
    rawSystem "git" ["commit", "-m", "First commit."]
    putStr "Pushing first commit.."
    rawSystem "git" ["push", "origin", "master"]
    putStrLn " done!"
    setCurrentDirectory ".."
    canonicalizePath wikiName


{-
 - Command: --init
 - -}
initWikiCommand :: [String] -> IO ()
initWikiCommand _ = do
    wikiName <- getWikiName
    initWiki wikiName
    run (RebuildWiki wikiName)
    return ()

{-
 - Creates a bare shared base repo, src/dest dirs, and wiki -config.yaml file.
 - -} 
initWiki :: WikiName -> IO Wiki
initWiki wikiName = do
    let repoPath = wikiName ++ ".git"
    repo <- initBareSharedRepo repoPath
    destDir <- createDestDir wikiName
    configFile <- createSetupFileFor wikiName [ ("wikiname", wikiName)
                                              , ("srcdir", wikiName)
                                              , ("destdir", "public_html/" ++ wikiName)
                                              , ("theme", "cayman")
                                              ]
    etcDir <- etcFilePath
    srcDir <- createSrcDir wikiName (etcDir ++ "/setup/auto-blog")
    installPostUpdateHook repo
    registerWiki wikiName configFile
    config <- loadWikiConfig wikiName
    return (Wiki wikiName config)


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

