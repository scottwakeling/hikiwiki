--
---- Copyright (c) 2016 Scott Wakeling - http://www.diskfish.org/
---- GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)
--
module Wiki
    (
      Wiki(Wiki),
      WikiCommand(InitWiki,RebuildWiki,RefreshWiki,RemoveWiki),
      WikiCommands(run),
      WikiConfig,
      WikiName
    ) where

import System.Directory
import System.FilePath ((</>))
import System.FilePath.Posix
import System.IO
import System.Process
import Text.Regex.Posix

import Compile
import Repo
import Yaml


type WikiName = String
type WikiConfig = [(String,String)]
data Wiki = Wiki
    { name :: WikiName
    , config :: WikiConfig
    }


{- Returns the filepath of the per-user hikiwiki wikilist file. -}
wikiListFilePath :: IO FilePath
wikiListFilePath = do
    home <- getHomeDirectory
    createDirectoryIfMissing False (home ++ "/.hikiwiki")
    return (home ++ "/.hikiwiki/wikilist")


loadWikiConfig :: WikiName -> IO WikiConfig
loadWikiConfig wikiName = do
    xs <- ((wikiConfigFilePath wikiName) >>= readFile)
    return (decodeYamlMarkdownHeader (lines xs))


wikiConfigFilePath :: WikiName -> IO FilePath
wikiConfigFilePath wikiName = do
    wikis <- getWikiList
    let wikiLine = filter (\wiki -> (wiki =~ ("^" ++ wikiName ++ " "))) wikis
    let line = head wikiLine
    return (keyValue (splitLine line))
  where
    keyValue (k,_,v) = v
    splitLine :: String -> (String,String,String)
    splitLine line = line =~ " "


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


{- Returns the contents of ~/.hikiwiki/wikilist -}
getWikiList :: IO [String]
getWikiList = do
    xs <- (wikiListFilePath >>= readFile)
    forceList xs
    return (lines xs)
  where
    forceList = foldr (\_ m -> m) (return ())


{-
 - Registers a wiki in ~/.hikiwiki/wikilist as a space-separated WikiName
 - and config filepath.
 - -}
registerWiki :: WikiName -> FilePath -> IO ()
registerWiki wikiName configFilePath = do
    wikiListFile <- getWikiListFile AppendMode
    hPutStrLn wikiListFile (wikiName ++ " " ++ configFilePath)
    hClose wikiListFile
    return ()


{- Unregisters a wiki by removing its entry from ~/.hikiwiki/wikilist -}
unregisterWiki :: WikiName -> IO ()
unregisterWiki wikiName = do
    wikis <- getWikiList
    let keepWikis = filter (\wiki -> not (wiki =~ ("^" ++ wikiName ++ " "))) wikis
    wikiListFilePath >>= removeFile
    newWikiListFile <- getWikiListFile WriteMode
    hPutStr newWikiListFile (unlines keepWikis)
    hClose newWikiListFile
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
compileWiki :: WikiConfig -> FilePath -> IO (Bool)
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
    themeName :: WikiConfig -> String
    themeName wikiConfig = case (lookupYaml "theme" wikiConfig) of
        Nothing -> "cayman"
        Just theme -> theme


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


{- A class of types that represent commands which can be run on a Wiki. -}
class WikiCommands c where
    run :: c -> IO ()

data WikiCommand = InitWiki WikiName
                 | RebuildWiki WikiName
                 | RefreshWiki WikiName
                 | RemoveWiki WikiName

instance WikiCommands WikiCommand where
    {- Creates a bare shared base repo, src + dest dirs, and config yaml. -}
    run (InitWiki wikiName) = do
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
        return ()

    {- Deletes the destination directory and recompiles the wiki specified. -}
    run (RebuildWiki wikiName) = do
        let publishDir = "public_html/" ++ wikiName
        publishDirExists <- doesDirectoryExist publishDir
        case publishDirExists of
            True    -> removeDirectoryRecursive publishDir
            False   -> putStrLn ("Creating " ++ publishDir)
        createDirectoryIfMissing True publishDir
        wikiConfig <- loadWikiConfig wikiName
        etcDir <- etcFilePath
        compileWiki wikiConfig etcDir
        return ()
    {- Rebuilds only source files modified since they were last built. -}
    run (RefreshWiki wikiNae) = do
        return ()
    {- Delete the base repo, config file, and src dir.
     - TODO: Also delete dest dir? Switch? -}
    run (RemoveWiki wikiName) = do
        putStrLn $ "Removing " ++ wikiName ++ " ..."
        removeDirectoryRecursive (wikiName ++ ".git")
        removeDirectoryRecursive wikiName
        removeFile (wikiName ++ "-config.yaml")
        unregisterWiki wikiName
        return ()

