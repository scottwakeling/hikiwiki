--
---- Copyright (c) 2015 Scott Wakeling - http://www.diskfish.org/
---- GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)
--
module Yaml
    (
      decodeYamlMarkdownHeader,
      decodeYamlLine,
      decodeYamlLines,
      lookupYaml
    ) where


import Control.Monad (forM)
import Data.List.Split
import System.Cmd
import System.Directory
import System.FilePath.Posix
import Text.Regex.Posix

--import Yaml.Internal


type MarkdownHeader = [String]
type YamlArray = [(String,String)]


{-
 - Decodes a line of yaml into a (key,value) tuple.
 - -}
decodeYamlLine :: String -> (String,String)
decodeYamlLine line = keyValue (splitLine line)
  where
    keyValue (k,_,v) = (k,v)
    splitLine :: String -> (String,String,String)
    splitLine line = line =~ ": "


{-
 - Decodes many lines of yaml into a list of (key,value) tuples.
 - -}
decodeYamlLines :: [String] -> YamlArray
decodeYamlLines lines = map decodeYamlLine lines


{-
 - Decodes a yaml markdown header, starting and ending with '---', into a
 - list of (key,value) tuples.
 - -}
decodeYamlMarkdownHeader :: MarkdownHeader -> YamlArray
decodeYamlMarkdownHeader lines =
    decodeYamlLines $ filter isYamlLine lines
  where
    isYamlLine :: String -> Bool
    isYamlLine line = (line =~ ": ")


{-
 - Lookup a value inside a list of (key,value) typles.
 - -}
lookupYaml :: String -> YamlArray -> Maybe String
lookupYaml key dict = lookup key dict




