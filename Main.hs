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

import Compile
import Yaml

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
 - extension. E.g. "/home/joe/hikiwiki.git" -> ("/home/joe/","hikiwiki",".git")
 -}
splitRepoPath :: FilePath -> (String,String,String)
splitRepoPath path = (snd (splitFileName path) =~ ".git")


getRepoName :: Repo -> String
getRepoName repo = repoName (splitRepoPath (getRepoLocation repo))
    where repoName (s,_,_) = s


{-
 - Wiki
 - -}
newtype WikiName = StringToWikiName {wikiNameToString :: String}
type Dictionary = [(String,String)]

data WikiConfig = WikiConfig
    { name :: WikiName
    , config :: Dictionary
    }


{-
 - Reads a wiki name frm stdin.
 -
 - TODO: Make this getWikiConfig, and ask for default theme etc.?
 - -}
getWikiName :: IO WikiName
getWikiName = do
    putStrLn "What will the wiki be named?"
    wikiName <- getLine
    return (StringToWikiName wikiName)


{-
 - Dispatch table, maps command line arguments to command processor functions.
 - -}
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("remove", removeWikiCommand) 
           , ("init", initWikiCommand)
           , ("rebuild", rebuildWikiCommand)
           , ("version", versionCommand)
           ]


{- 
 - Command: --remove wiki
 - Delete the base repo, config file, and src dir.
 - TODO: Also delete dest dir? Switch?
 -      Remove and rebuild should take a Wiki in general, split the commands
 -      out from the action code. I know both parts are impure, but it gives
 -      you a typed interface instread of just [String] everywhere..
 - -}
removeWikiCommand :: [String] -> IO ()
removeWikiCommand [wiki] = do
    removeWiki (StringToWikiName wiki)


removeWiki :: WikiName -> IO ()
removeWiki wikiName = do
    let wiki = wikiNameToString wikiName
    putStrLn $ "Removing " ++ wiki ++ " ..."
    removeDirectoryRecursive (wiki ++ ".git")
    removeDirectoryRecursive wiki
    removeFile (wiki ++ "-config.yaml")
    unregisterWiki wikiName


{-
 - Command: --rebuild wiki
 - -}
rebuildWikiCommand :: [String] -> IO ()
rebuildWikiCommand [wiki] = do
    rebuildWiki (StringToWikiName wiki)
    return ()


{-
 - Deletes the destination directory and recompiles the wiki specified.
 - -}
rebuildWiki :: WikiName -> IO Bool
rebuildWiki wikiName = do
    let wiki = wikiNameToString wikiName
    let publishDir = "public_html/" ++ wiki
    publishDirExists <- doesDirectoryExist publishDir
    case publishDirExists of
        True    -> removeDirectoryRecursive publishDir
        False   -> putStrLn ("Creating " ++ publishDir)
    createDirectoryIfMissing True publishDir
    wikiConfig <- loadWikiConfig wikiName
    etcDir <- etcFilePath
    compileWiki wikiConfig etcDir


{- Creates a bare shared repo at the location specified.
 - -}
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
createSetupFileFor :: WikiName -> [(String,String)] -> IO FilePath
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
    configFile = wikiNameToString wikiName ++ "-config.yaml"


{-
 - Creates a dest dir in public_html.
 - TODO: Assumes public_html is in . Should read it from config, or be passed
 -       the 'publish dir' or something?
 - -}
createDestDir :: WikiName -> IO FilePath
createDestDir wikiName = do
    let destDir = "public_html/" ++ wikiNameToString wikiName
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
    let wiki = wikiNameToString wikiName
    rawSystem "git" ["clone", (wiki ++ ".git"), wiki]
    putStrLn "Copying base template to src dir.."
    rawSystem "cp" ["-r",  (etc ++ "/."), wiki]
    setCurrentDirectory wiki
    rawSystem "git" ["add", "--all"]
    rawSystem "git" ["commit", "-m", "First commit."]
    putStr "Pushing first commit.."
    rawSystem "git" ["push", "origin", "master"]
    putStrLn " done!"
    setCurrentDirectory ".."
    canonicalizePath wiki


{-
 - Returns the filepath of the per-user hikiwiki wikilist file.
 - -}
wikiListFilePath :: IO FilePath
wikiListFilePath = do
    home <- getHomeDirectory
    createDirectoryIfMissing False (home ++ "/.hikiwiki")
    return (home ++ "/.hikiwiki/wikilist")


{-
 - Returns the filepath of the per-user hikiwiki etc folder.
 - TODO: Install ~/.hikiwiki/etc and its default contents.
 - -}
etcFilePath :: IO FilePath
etcFilePath = do
    home <- getHomeDirectory
    return (home ++ "/.hikiwiki/etc")


{-
 - Returns a file descriptor for ~/.hikiwiki/wikilist with the specified
 - privileges. Creates ~/.hikiwiki dir and wikilist file if they don't exist
 - yet.
 - -}
getWikiListFile :: IOMode -> IO Handle
getWikiListFile ioMode = do
    wikiListFile <- wikiListFilePath
    openFile wikiListFile ioMode


{-
 - Registers a wiki in ~/.hikiwiki/wikilist as a space-separated WikiName
 - and config filepath.
 - -}
registerWiki :: WikiName -> FilePath -> IO ()
registerWiki wikiName configFilePath = do
    wikiListFile <- getWikiListFile AppendMode
    hPutStrLn wikiListFile ((wikiNameToString wikiName) ++ " " ++ configFilePath)
    hClose wikiListFile
    return ()


{-
 - Unregisters a wiki by removing its entry from ~/.hikiwiki/wikilist
 - -}
unregisterWiki :: WikiName -> IO ()
unregisterWiki wikiName = do
    wikis <- getWikiList
    let dropWiki = wikiNameToString wikiName
    let keepWikis = filter (\wiki -> not (wiki =~ ("^" ++ dropWiki ++ " "))) wikis
    wikiListFilePath >>= removeFile
    newWikiListFile <- getWikiListFile WriteMode
    hPutStr newWikiListFile (unlines keepWikis)
    hClose newWikiListFile
    return ()


{-
 - Returns the contents of ~/.hikiwiki/wikilist
 - -}
getWikiList :: IO [String]
getWikiList = do
    xs <- (wikiListFilePath >>= readFile)
    forceList xs
    return (lines xs)
  where
    forceList = foldr (\_ m -> m) (return ())


{-
 - Command: --init
 - -}
initWikiCommand :: [String] -> IO ()
initWikiCommand _ = do
    wikiName <- getWikiName
    initWiki wikiName
    rebuildWiki wikiName
    return ()

{-
 - Creates a bare shared base repo, src/dest dirs, and wiki -config.yaml file.
 - -} 
initWiki :: WikiName -> IO WikiConfig
initWiki wikiName = do
    let repoPath = (wikiNameToString wikiName ++ ".git")
    repo <- initBareSharedRepo repoPath
    destDir <- createDestDir wikiName
    let wiki = wikiNameToString wikiName
    configFile <- createSetupFileFor wikiName [ ("wikiname", wiki)
                                              , ("srcdir", wiki)
                                              , ("destdir", "public_html/" ++ wiki)
                                              , ("theme", "cayman")
                                              ]
    etcDir <- etcFilePath
    srcDir <- createSrcDir wikiName (etcDir ++ "/setup/auto-blog")
    installPostUpdateHook repo
    registerWiki wikiName configFile
    config <- loadWikiConfig wikiName
    return (WikiConfig wikiName config)


loadWikiConfig :: WikiName -> IO Dictionary
loadWikiConfig wikiName = do
    xs <- ((wikiConfigFilePath wikiName) >>= readFile)
    return (decodeYamlMarkdownHeader (lines xs))


wikiConfigFilePath :: WikiName -> IO FilePath
wikiConfigFilePath wikiName = do
    wikis <- getWikiList
    let findWiki = wikiNameToString wikiName
    let wikiLine = filter (\wiki -> (wiki =~ ("^" ++ findWiki ++ " "))) wikis
    let line = head wikiLine
    return (keyValue (splitLine line))
  where
    keyValue (k,_,v) = v
    splitLine :: String -> (String,String,String)
    splitLine line = line =~ " "


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

