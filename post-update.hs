import System.IO
import System.Process
import System.Directory

{--
 - HikiWiki 'post-update' hook. Installed by default in all HikiWiki repos.
 - Run when incoming changes are received from a push. Issues a rebuild.
 -
 - The name of each refspec receiving changes is passed in args.
 -
 - TODO: When does HikiWiki care about branches other than master?
 - TODO: Only rebuild if changes arrive for the current 'active' branch?
--}
main :: IO ()
main = do
  setCurrentDirectory "../../../"
  --putStrLn getCurrentDirectory
  rawSystem "./HikiWiki" ["--rebuild", "r1"]
  return ()



