--
---- Copyright (c) 2016 Scott Wakeling - http://www.diskfish.org/
---- GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)
--
module Repo
    (
      getRepoName,
      IsBare(IsBare),
      IsShared(IsShared),
      Repo(getRepoLocation,LocalRepo)
    ) where

import Data.List.Utils
import System.Console.GetOpt
import System.FilePath ((</>))
import System.FilePath.Posix
import Text.Regex.Posix


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

