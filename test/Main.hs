import Test.HUnit

import Yaml

test_decodeYamlLine :: Test
test_decodeYamlLine =
    TestCase $ assertEqual "Decode yaml into (key,value)"
    ("wikiname","testwiki") $ decodeYamlLine "wikiname: testwiki"


test_decodeYamlLines :: Test
test_decodeYamlLines =
    TestCase $ assertEqual "Decode list of yaml lines into list of\
                            \(key,value) tuples."
    [ ("wikiname","testwiki")
    , ("srcdir", "/home/test/testwiki")
    ]
    (decodeYamlLines [ "wikiname: testwiki"
                     , "srcdir: /home/test/testwiki"
                     ])


test_decodeYamlMarkdownHeader :: Test
test_decodeYamlMarkdownHeader =
    TestCase $ assertEqual "Decode only the yaml lines from a markdown header\
                            \that starts and stops with '---'"
    [ ("wikiname","testwiki")
    , ("srcdir", "/home/test/testwiki")
    ]
    (decodeYamlMarkdownHeader [ "---"
                              , "wikiname: testwiki"
                              , "srcdir: /home/test/testwiki"
                              , "---"
                              ])


test_lookupYaml :: Test
test_lookupYaml = 
    TestCase $ assertEqual "Failed to look up value for an existing key."
    (Just "bar") $ lookupYaml "foo" [("key","value"),("foo","bar")]


main :: IO Counts
main = runTestTT $ TestList [ test_decodeYamlLines
                            , test_decodeYamlLine
                            , test_decodeYamlMarkdownHeader
                            , test_lookupYaml
                            ]
