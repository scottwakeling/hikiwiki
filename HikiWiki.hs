import System.IO
import System.Cmd
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.Directory
import Data.Char(toUpper)
import Data.Maybe (fromMaybe)

getWikiName :: IO String
getWikiName = do
              putStrLn "What will the wiki be named?"
              getLine

getAdminUser :: IO String
getAdminUser = do
               putStrLn "What user will be admin?"
               getLine

writeSetupFile :: String -> String -> IO ()
writeSetupFile wikiName adminUser =
        do putStrLn $ "Setting up " ++ wikiName ++ " ..."   
           outh <- openFile (wikiName ++ ".setup") WriteMode
           hPutStrLn outh "# HikiWiki::Setup::Yaml - YAML formatted setup file"
           hPutStrLn outh "#"
           hPutStrLn outh "# Setup file for HikiWiki."
           hPutStrLn outh "#"
           hPutStrLn outh "# Passing this to HikiWiki --setup will make HikiWiki generate"
           hPutStrLn outh "# wrappers and build the wiki."
           hPutStrLn outh "#"
           hPutStrLn outh "# Remember to re-run HikiWiki --setup any time you edit this file."
           hPutStrLn outh "#"
           hPutStrLn outh "# name of the wiki"
           hPutStrLn outh ("wikiname: " ++ wikiName)
           hPutStrLn outh "# users who are wiki admins"
           hPutStrLn outh "adminuser:"
           hPutStrLn outh ("- " ++ adminUser)
           hClose outh

initBareRepo :: String -> IO ()
initBareRepo wikiName =
        do rawSystem "git" ["init", "--bare", "--shared", (wikiName ++ ".git")]
           return ()

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("--remove", removeWiki) 
           , ("--init", initWiki)
           ]

removeWiki :: [String] -> IO ()
removeWiki [repoName] = do
  putStrLn $ "Removing repo " ++ repoName ++ " ..."
  removeDirectoryRecursive $ repoName ++ ".git"

setupRepo :: String -> String -> IO ()
setupRepo repoName setupDir = do
  rawSystem "cp" ["-r",  setupDir, repoName]
  rawSystem "git" ["init", "--shared", repoName]
  setCurrentDirectory repoName
  rawSystem "git" ["add", "--all"]
  rawSystem "git" ["commit", "-m", "First commit."]
  return ()

cloneRepo :: String -> String -> IO ()
cloneRepo src dst = do
  rawSystem "git" ["clone", src, dst]
  return ()

initWiki :: [String] -> IO ()
initWiki [] = do
  wikiName <- getWikiName
  adminUser <- getAdminUser
  setupRepo (wikiName ++ ".git") "etc/setup/auto-blog"
  return ()

main :: IO ()
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args





