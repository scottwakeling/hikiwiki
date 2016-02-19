--
---- Copyright (c) 2016 Scott Wakeling - http://www.diskfish.org/
---- GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)
--
module Wiki
    (
      etcFilePath,
      loadWikiConfig,
      registerWiki,
      Wiki(Wiki),
      WikiCommand(RebuildWiki,RefreshWiki,RemoveWiki),
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


{- A class of types that represent commands which can be run on a Wiki. -}
class WikiCommands c where
    run :: c -> IO ()

data WikiCommand = RebuildWiki WikiName
                 | RefreshWiki WikiName
                 | RemoveWiki WikiName

instance WikiCommands WikiCommand where
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

