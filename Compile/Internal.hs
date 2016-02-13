module Compile.Internal where


import Control.Monad (forM)
import System.Directory
import System.FilePath.Posix
import Text.Regex.Posix


splitFilePath :: FilePath -> String -> (String,String,String)
splitFilePath path ext = (snd (splitFileName path) =~ ext)


isMarkdown :: FilePath -> Bool
isMarkdown path = case (splitFilePath path ".mdwn") of
    (_,".mdwn","") -> True
    _ -> False


isImage :: FilePath -> Bool
isImage path = case (splitFilePath path ".jpg") of
    (_,".jpg","") -> True
    _ -> False

