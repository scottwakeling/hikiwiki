import Test.HUnit

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


testSafeHeadForEmptyList :: Test
testSafeHeadForEmptyList = 
    TestCase $ assertEqual "Should return Nothing for empty list"
    Nothing (safeHead ([]::[Int]))


testSafeHeadForNonEmptyList :: Test
testSafeHeadForNonEmptyList = 
    TestCase $ assertEqual "Should return (Just head) for non empty list"
    (Just 1) (safeHead ([1]::[Int]))


pushTest :: Test
pushTest = TestCase $ assertEqual "Pushing to an empty list should give length=1"
    1 (length (1 : []))


main :: IO Counts
main = runTestTT $ TestList [ testSafeHeadForEmptyList
                            , testSafeHeadForNonEmptyList
                            , pushTest
                            ]
