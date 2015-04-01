import System.IO
import System.Cmd
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.Directory
import Data.Char(toUpper)
import Data.Maybe (fromMaybe)
import Data.List.Utils
import Text.Regex.Posix
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.FilePath.Posix


{- Repo -}
newtype IsBare = IsBare Bool
newtype IsShared = IsShared Bool

data Repo = LocalRepo
  { getRepoLocation :: FilePath
  , isBare :: IsBare
  , isShared :: IsShared
  }

getRepoName :: Repo -> String
getRepoName repo = repoName (splitRepoPath (getRepoLocation repo))
    where repoName (s,_,_) = s

splitRepoPath :: FilePath -> (String,String,String)
splitRepoPath path = (snd (splitFileName path) =~ ".git")


{-
 - Wiki
 - -}
newtype WikiName = StringToWikiName {wikiNameToString :: String}

data Wiki = LocalWiki
  { name :: WikiName
  , baseRepo :: Repo
  , srcDir :: FilePath
  , dstDir :: FilePath
  , config :: FilePath
  }


{-
 - Reads a wiki name frm stdin.
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
dispatch = [ ("--remove", removeWiki) 
           , ("--init", initWiki)
           , ("--rebuild", rebuildWiki)
           , ("--version", version)
           ]


{- 
 - Command: --remove wiki
 - Delete the base repo, config file, and src dir.
 - TODO: Also delete dest dir? Switch?
 -      Remove and rebuild should take a Wiki in general, split the commands
 -      out from the action code. I know both parts are impure, but it gives
 -      you a typed interface instread of just [String] everywhere..
 - -}
removeWiki :: [String] -> IO ()
removeWiki [wiki] = do
  putStrLn $ "Removing " ++ wiki ++ " ..."
  removeDirectoryRecursive $ wiki ++ ".git"
  removeDirectoryRecursive wiki
  removeFile (wiki ++ "-config.yaml")


{-
 - Command: --rebuild wiki
 - Deletes the destination directory and recompiles the wiki.
 - TOOD: Separate out command code into a typed function that takes a Wiki?
 - -}
rebuildWiki :: [String] -> IO ()
rebuildWiki [wiki] = do
  removeDirectoryRecursive $ "public_html/" ++ wiki
  createDirectoryIfMissing False $ "public_html/" ++ wiki
  compileWiki wiki
  return ()


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
    if isDirectory
      then getSrcFilesRecursive path
      else return [path]
  return (concat paths)


{-
 - Compiles input .mdwn file to output .html file.
 - -}
compile :: FilePath -> FilePath -> IO ()
compile input output = do
  putStrLn $ "Compiling " ++ input ++ " to " ++ output
  createDirectoryIfMissing True (fst (splitFileName output))
  rawSystem "pandoc" ["-o", output, input]
  return ()


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
 - TODO: Assumes the src dir and public_html are in .
 -       Should take a Wiki?
 -       Should only pass source files (e.g. .mdwn) to compileSrc, not
 -       everything in the location provided..
 - -}
compileWiki :: FilePath -> IO ()
compileWiki wiki = do
  src <- getSrcFilesRecursive wiki
  compileSrc src
  return ()


{- Creates a bare shared repo at the location specified.
 - -}
initBareSharedRepo :: FilePath -> IO Repo
initBareSharedRepo repoLocation = do
  rawSystem "git" ["init", repoLocation, "--bare", "--shared"]
  return (LocalRepo repoLocation (IsBare True) (IsShared True))


{-
 - Creates and chmod ugo+x a post-update hook shell script in the Repo
 - provided.
 - -}
installPostUpdateHook :: Repo -> IO ()
installPostUpdateHook repo = do
  let updateHookPath = (getRepoLocation repo ++ "/hooks/post-update")
  fd <- openFile updateHookPath WriteMode
  hPutStrLn fd "#!/bin/sh\n"
  hPutStrLn fd "cd .."
  hPutStrLn fd ("./HikiWiki --rebuild " ++ getRepoName repo)
  hClose fd
  perms <- getPermissions updateHookPath
  setPermissions updateHookPath (perms {executable = True})


{-
 - Creates a $WIKI-setup.yaml file containing the key,value pairs provided.
 - -}
createSetupFileFor :: WikiName -> [(String,String)] -> IO ()
createSetupFileFor wikiName settings = do
  fd <- openFile (wikiNameToString wikiName ++ "-config.yaml") WriteMode
  hPutStrLn fd "# HikiWiki - YAML formatted config file\n"
  mapM (writeSetupFileLine fd) settings
  hClose fd
      where writeSetupFileLine fd setting = do
                hPutStrLn fd (fst setting ++ ": " ++ snd setting)


{-
 - Creates a dest dir in public_html.
 - TODO: Assumes public_html is in . Should read it from config, or be passed
 -       the 'publish dir' or something?
 - -}
createDestDir :: WikiName -> IO ()
createDestDir wikiName = do
  createDirectoryIfMissing True $ "public_html/" ++ wikiNameToString wikiName
  return ()


{-
 - Creates a src dir by cloning a base repo in the current directory.
 - TODO: Assumes repo is in . and named after WikiName.
 -       Should take a Repo/RepoLocation and WikiName?
 -       Or, gets a wiki config and src dir and repo loc is in there..?
 - -}
createSrcDir :: WikiName -> FilePath -> IO ()
createSrcDir wikiName etc = do
  let wiki = wikiNameToString wikiName
  rawSystem "git" ["clone", (wiki ++ ".git"), wiki]
  rawSystem "cp" ["-r",  (etc ++ "/."), wiki]
  setCurrentDirectory wiki
  rawSystem "git" ["add", "--all"]
  rawSystem "git" ["commit", "-m", "First commit."]
  rawSystem "git" ["push", "origin", "master"]
  return ()


{-
 - Command: --init
 - Creates a bare shared base repo, src/dest dirs, and wiki -config.yaml file.
 - TODO: As for other commands, separate command function from internals for a
 -       typed interface. Probably return a Wiki too..
 - -}
initWiki :: [String] -> IO ()
initWiki [] = do
  wikiName <- getWikiName
  let repoPath = (wikiNameToString wikiName ++ ".git")
  installPostUpdateHook <- initBareSharedRepo repoPath
  createDestDir wikiName
  let wiki = wikiNameToString wikiName
  createSetupFileFor wikiName [ ("wikiname", wiki)
                              , ("srcdir", wiki)
                              , ("destdir", "public_html/" ++ wiki)
                              ]
  createSrcDir wikiName "etc/setup/auto-blog"
  return ()


{-
 - Prints a simple version string.
 - -}
printVersionString :: IO ()
printVersionString = putStrLn "HikiWiki v0.1"


{-
 - Command: --version
 - Prints a simple version string.
 - -}
version :: [String] -> IO ()
version [] = printVersionString


{-
 - Main entry point.
 - Dispatches command line arguments to command functions.
 - -}
main :: IO ()
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args


