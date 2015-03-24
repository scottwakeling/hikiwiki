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
  setCurrentDirectory ".."
  return ()

rebuildWiki :: [String] -> IO ()
rebuildWiki [repo] = do
  removeDirectoryRecursive $ "public_html/" ++ repo
  createDirectoryIfMissing False $ "public_html/" ++ repo
  compileWiki repo
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

-- wiki is without the .git
-- repo is with the .git

compileSrc :: [String] -> String -> String -> IO ()
compileSrc [] _ _ = do
  return ()
compileSrc (x:xs) repo outputFolder = do
  compile x (replace ".mdwn" ".html" (replace repo outputFolder x))
  compileSrc xs repo outputFolder
  return ()

compileWiki :: String -> IO ()
compileWiki repo = do
  src <- getSrcFilesRecursive $ repo ++ ".git"
  compileSrc src (repo ++ ".git") ("public_html/" ++ repo)
  return ()
  --  - pandoc -o $dst/$relativepath/$inputfile $relativepath/$inputfile
  -- All input files are converted and written to dst.

initWiki :: [String] -> IO ()
initWiki [] = do
  wiki <- getWikiName
  adminUser <- getAdminUser
  setupRepo (wiki ++ ".git") "etc/setup/auto-blog"
  createDirectoryIfMissing True $ "public_html/" ++ wiki
  rebuildWiki [wiki]
  return ()

main :: IO ()
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args





