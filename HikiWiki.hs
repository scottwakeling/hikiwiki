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

getWikiName :: IO String
getWikiName = do
              putStrLn "What will the wiki be named?"
              getLine

getAdminUser :: IO String
getAdminUser = do
               putStrLn "What user will be admin?"
               getLine

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("--remove", removeWiki) 
           , ("--init", initWiki)
           , ("--rebuild", rebuildWiki)
           , ("--version", version)
           ]

removeWiki :: [String] -> IO ()
removeWiki [wiki] = do
  putStrLn $ "Removing " ++ wiki ++ " ..."
  removeDirectoryRecursive $ wiki ++ ".git"
  removeDirectoryRecursive wiki
  removeFile (wiki ++ "-config.yaml")

rebuildWiki :: [String] -> IO ()
rebuildWiki [wiki] = do
  removeDirectoryRecursive $ "public_html/" ++ wiki
  createDirectoryIfMissing False $ "public_html/" ++ wiki
  compileWiki wiki
  return ()

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

compile :: String -> String -> IO ()
compile input output = do
  putStrLn $ "Compiling " ++ input ++ " to " ++ output
  createDirectoryIfMissing True (fst (splitFileName output))
  writeFile output "foobar"
  rawSystem "pandoc" ["-o", output, input]
  return ()

compileSrc :: [String] -> String -> String -> IO ()
compileSrc [] _ _ = do
  return ()
compileSrc (x:xs) repo outputFolder = do
  compile x (replace ".mdwn" ".html" ("public_html/" ++ x))
  compileSrc xs repo outputFolder
  return ()

compileWiki :: String -> IO ()
compileWiki wiki = do
  src <- getSrcFilesRecursive wiki
  compileSrc src (wiki ++ ".git") ("public_html/" ++ wiki)
  return ()

initBareSharedRepo :: FilePath -> IO ()
initBareSharedRepo repo = do
  rawSystem "git" ["init", repo, "--bare", "--shared"]
  return ()

installPostUpdateHook :: String -> IO ()
installPostUpdateHook wiki = do
  let updateHookPath = (wiki ++ ".git/hooks/post-update")
  fd <- openFile updateHookPath WriteMode
  hPutStrLn fd "#!/bin/sh\n"
  hPutStrLn fd "cd .."
  hPutStrLn fd ("./HikiWiki --rebuild " ++ wiki)
  hClose fd
  perms <- getPermissions updateHookPath
  setPermissions updateHookPath (perms {executable = True})

writeSetupFileLine :: Handle -> (String,String) -> IO ()
writeSetupFileLine fd setting = do
  hPutStrLn fd (fst setting ++ ": " ++ snd setting)

createSetupFileFor :: String -> [(String,String)] -> IO ()
createSetupFileFor wiki settings = do
  fd <- openFile (wiki ++ "-config.yaml") WriteMode
  hPutStrLn fd "# HikiWiki - YAML formatted config file\n"
  mapM (writeSetupFileLine fd) settings
  hClose fd

createDestDir :: String -> IO ()
createDestDir wiki = do
  createDirectoryIfMissing True $ "public_html/" ++ wiki
  return ()

createSrcDir :: String -> FilePath -> IO ()
createSrcDir wiki etc = do
  rawSystem "git" ["clone", (wiki ++ ".git"), wiki]
  rawSystem "cp" ["-r",  (etc ++ "/."), wiki]
  setCurrentDirectory wiki
  rawSystem "git" ["add", "--all"]
  rawSystem "git" ["commit", "-m", "First commit."]
  rawSystem "git" ["push", "origin", "master"]
  return ()

initWiki :: [String] -> IO ()
initWiki [] = do
  wiki <- getWikiName
  adminUser <- getAdminUser
  let repo = (wiki ++ ".git")
  initBareSharedRepo repo
  installPostUpdateHook wiki
  createDestDir wiki
  createSetupFileFor wiki [ ("wikiname", wiki)
                          , ("srcdir", wiki)
                          , ("destdir", "public_html/" ++ wiki)
                          ]
  createSrcDir wiki "etc/setup/auto-blog"
  return ()

version :: [String] -> IO ()
version [] = do
  putStrLn "HikiWiki v0.1"

main :: IO ()
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args





