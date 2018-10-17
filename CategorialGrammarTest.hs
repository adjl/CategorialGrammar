import Test.HUnit
import CategorialGrammar hiding (main)

-- Test Lexicon
tJohn    = makeWord "John"    (Category "n")
tPoor    = makeWord "poor"    (makeForward "n" "n")
tWalks   = makeWord "walks"   (makeBackward "n" "S")
tLoves   = makeWord "loves"   (Forward  (makeBackward "n" "S") (Category "n"))
tQuickly = makeWord "quickly" (Backward (makeBackward "n" "S") (makeBackward "n" "S"))

tPhrase :: [Word]
tPhrase = [tPoor, tJohn, tWalks, tQuickly]

----------------------------------------------------------

testCategory :: Test
testCategory =
    TestCase $ assertEqual "Category 'n'"
        "n"
        (show $ Category "n")

testForward :: Test
testForward =
    TestCase $ assertEqual "Forward (Category 'n') (Category 's')"
        "n/S"
        (show $ Forward (Category "n") (Category "S"))

testBackward :: Test
testBackward =
    TestCase $ assertEqual "Backward (Category 'n') (Category 's')"
        "n\\S"
        (show $ Backward (Category "n") (Category "S"))

testMakeForward :: Test
testMakeForward =
    TestCase $ assertEqual "makeForward 'n' 's'"
        "n/S"
        (show $ makeForward "n" "S")

testMakeBackward :: Test
testMakeBackward =
    TestCase $ assertEqual "makeBackward 'n' 's'"
        "n\\S"
        (show $ makeBackward "n" "S")

testNestedMixed :: Test
testNestedMixed =
    TestCase $ do
        assertEqual "Forward (makeBackward 'n' 's') (Category 's')"
            "(n\\S)/S"
            (show $ Forward (makeBackward "n" "S") (Category "S"))
        assertEqual "Backward (Category 'n') (makeForward 'n' 's')"
            "n\\(n/S)"
            (show $ Backward (Category "n") (makeForward "n" "S"))
        assertEqual "Backward (makeForward 'n' 's') (makeBackward 'n' 's')"
            "(n/S)\\(n\\S)"
            (show $ Backward (makeForward "n" "S") (makeBackward "n" "S"))

testNestedDeep :: Test
testNestedDeep =
    TestCase $ assertEqual "((n/S)\\(S\\S))/((n\\n)\\(S/n))"
        "((n/S)\\(S\\S))/((n\\n)\\(S/n))"
        (show $ Forward (Backward (makeForward  "n" "S") (makeBackward "S" "S"))
                        (Backward (makeBackward "n" "n") (makeForward  "S" "n")))

testWord :: Test
testWord =
    TestCase $ do
        assertEqual "Word 'John' n"
            "\"John\"::n"
            (show tJohn)
        assertEqual "Word 'walks' n\\S"
            "\"walks\"::n\\S"
            (show tWalks)
        assertEqual "Word 'quickly' (n\\S)\\(n\\S)"
            "\"quickly\"::(n\\S)\\(n\\S)"
            (show tQuickly)

testIsForward :: Test
testIsForward =
    TestCase $ do
        assertEqual "isForward n/n"
            True
            (isForward tPoor)
        assertEqual "isForward (n\\S)/n"
            True
            (isForward tLoves)
        assertEqual "isForward n"
            False
            (isForward tJohn)
        assertEqual "isForward (n\\S)"
            False
            (isForward tWalks)
        assertEqual "isForward (n\\S)\\(n\\S)"
            False
            (isForward tQuickly)

testIsBackward :: Test
testIsBackward =
    TestCase $ do
        assertEqual "isBackward n\\S"
            True
            (isBackward tWalks)
        assertEqual "isBackward (n\\S)\\(n\\S)"
            True
            (isBackward tQuickly)
        assertEqual "isBackward n"
            False
            (isBackward tJohn)
        assertEqual "isBackward n/n"
            False
            (isBackward tPoor)
        assertEqual "isBackward (n\\S)/n"
            False
            (isBackward tLoves)

testGetLeftCat :: Test
testGetLeftCat =
    TestCase $ do
        assertEqual "getLeftCat $ Word 'walks' n\\S"
            "n"
            (show $ getLeftCat tWalks)
        assertEqual "getLeftCat $ Word 'quickly' (n\\S)\\(n\\S)"
            "n\\S"
            (show $ getLeftCat tQuickly)

testGetRightCat :: Test
testGetRightCat =
    TestCase $ do
        assertEqual "getRightCat $ Word 'walks' n\\S"
            "S"
            (show $ getRightCat tWalks)
        assertEqual "getRightCat $ Word 'quickly' (n\\S)\\(n\\S)"
            "n\\S"
            (show $ getRightCat tQuickly)

testCanReduceLeft :: Test
testCanReduceLeft =
    TestCase $ do
        assertEqual "canReduceLeft n n\\S"
            True
            (canReduceLeft tJohn tWalks)
        assertEqual "canReduceLeft n\\S (n\\S)\\(n\\S)"
            True
            (canReduceLeft tWalks tQuickly)
        assertEqual "canReduceLeft n\\S n\\S"
            False
            (canReduceLeft tWalks tWalks)
        assertEqual "canReduceLeft (n\\S)\\(n\\S) (n\\S)\\(n\\S)"
            False
            (canReduceLeft tQuickly tQuickly)
        assertEqual "canReduceLeft n/n n"
            False
            (canReduceLeft tPoor tJohn)
        assertEqual "canReduceLeft (n\\S)/n n"
            False
            (canReduceLeft tLoves tJohn)

testCanReduceRight :: Test
testCanReduceRight =
    TestCase $ do
        assertEqual "canReduceRight n/n n"
            True
            (canReduceRight tPoor tJohn)
        assertEqual "canReduceRight (n\\S)/n n"
            True
            (canReduceRight tLoves tJohn)
        assertEqual "canReduceRight n/n n/n"
            False
            (canReduceRight tPoor tPoor)
        assertEqual "canReduceRight (n\\S)/n (n\\S)/n"
            False
            (canReduceRight tLoves tLoves)
        assertEqual "canReduceRight n n\\S"
            False
            (canReduceRight tJohn tWalks)
        assertEqual "canReduceRight n\\S (n\\S)\\(n\\S)"
            False
            (canReduceRight tWalks tQuickly)

testCanReducePair :: Test
testCanReducePair =
    TestCase $ do
        assertEqual "canReducePair n n\\S"
            True
            (canReducePair tJohn tWalks)
        assertEqual "canReducePair n\\S (n\\S)\\(n\\S)"
            True
            (canReducePair tWalks tQuickly)
        assertEqual "canReducePair n/n n"
            True
            (canReducePair tPoor tJohn)
        assertEqual "canReducePair (n\\S)/n n"
            True
            (canReducePair tLoves tJohn)

testCanReduceAdjacent :: Test
testCanReduceAdjacent =
    TestCase $ do
        assertEqual "John quickly"
            [False]
            (canReduceAdjacent [tJohn, tQuickly])
        assertEqual "poor John"
            [True]
            (canReduceAdjacent [tPoor, tJohn])
        assertEqual "poor John quickly"
            [True, False]
            (canReduceAdjacent [tPoor, tJohn, tQuickly])
        assertEqual "poor walks quickly"
            [False, True]
            (canReduceAdjacent [tPoor, tWalks, tQuickly])
        assertEqual "John poor walks loves quickly"
            [False, False, False, False]
            (canReduceAdjacent [tJohn, tPoor, tWalks, tLoves, tQuickly])
        assertEqual "poor John walks quickly"
            [True, True, True]
            (canReduceAdjacent tPhrase)

testBranchingPoints :: Test
testBranchingPoints =
    TestCase $ do
        assertEqual "John quickly"
            (StackState [tJohn, tQuickly] [])
            (branchingPoints [tJohn, tQuickly])
        assertEqual "John walks"
            (StackState [tJohn, tWalks] [0])
            (branchingPoints [tJohn, tWalks])
        assertEqual "poor John quickly"
            (StackState [tPoor, tJohn, tQuickly] [0])
            (branchingPoints [tPoor, tJohn, tQuickly])
        assertEqual "poor walks quickly"
            (StackState [tPoor, tWalks, tQuickly] [1])
            (branchingPoints [tPoor, tWalks, tQuickly])
        assertEqual "poor John walks"
            (StackState [tPoor, tJohn, tWalks] [0, 1])
            (branchingPoints [tPoor, tJohn, tWalks])

testNextBranch :: Test
testNextBranch =
    TestCase $ do
        assertEqual "Get next branch on empty stack"
            (-1, [], [])
            (nextBranch [])
        assertEqual "Empty stack after empty state"
            (-1, [], [])
            (nextBranch [(StackState tPhrase [])])
        assertEqual "Empty stack after empty states"
            (-1, [], [])
            (nextBranch [(StackState tPhrase []),
                         (StackState tPhrase [])])
        assertEqual "Get next branch with no remaining indices"
            (0, tPhrase, [(StackState tPhrase [])])
            (nextBranch  [(StackState tPhrase [0])])
        assertEqual "Get next branch with remaining indices"
            (0, tPhrase, [(StackState tPhrase [1, 2])])
            (nextBranch  [(StackState tPhrase [0..2])])
        assertEqual "Get next branch after empty state"
            (0, tPhrase, [(StackState tPhrase [1, 2])])
            (nextBranch  [(StackState tPhrase []),
                          (StackState tPhrase [0..2])])
        assertEqual "Get next branch before empty state"
            (0, tPhrase, [(StackState tPhrase []),
                          (StackState tPhrase [])])
            (nextBranch  [(StackState tPhrase [0]),
                          (StackState tPhrase [])])
        assertEqual "Get next branch between empty states"
            (0, tPhrase, [(StackState tPhrase [1, 2]),
                          (StackState tPhrase [])])
            (nextBranch [(StackState tPhrase []),
                         (StackState tPhrase [0..2]),
                         (StackState tPhrase [])])

testConcatText :: Test
testConcatText =
    TestCase $ do
        assertEqual "John ++ walks"
            "John walks"
            (concatText tJohn tWalks)
        assertEqual "poor John ++ walks"
            "poor John walks"
            (concatText (makeWord "poor John" (Category "n")) tWalks)
        assertEqual "John ++ walks quickly"
            "John walks quickly"
            (concatText tJohn (makeWord "walks quickly" (makeBackward "n" "S")))
        assertEqual "poor John ++ walks quickly"
            "poor John walks quickly"
            (concatText (makeWord "poor John" (Category "n"))
                        (makeWord "walks quickly" (makeBackward "n" "S")))

testReducePair :: Test
testReducePair =
    TestCase $ do
        assertEqual "John, walks"
            "\"John walks\"::S"
            (show $ reducePair tJohn tWalks)
        assertEqual "poor John, walks"
            "\"poor John walks\"::S"
            (show $ reducePair (makeWord "poor John" (Category "n")) tWalks)
        assertEqual "John, walks quickly"
            "\"John walks quickly\"::S"
            (show $ reducePair tJohn (makeWord "walks quickly" (makeBackward "n" "S")))
        assertEqual "poor John, walks quickly"
            "\"poor John walks quickly\"::S"
            (show $ reducePair (makeWord "poor John" (Category "n"))
                               (makeWord "walks quickly" (makeBackward "n" "S")))

testReducePairInPhrase :: Test
testReducePairInPhrase =
    TestCase $ do
        assertEqual "John walks"
            [(makeWord "John walks" (Category "S"))]
            (reducePairInPhrase 0 [tJohn, tWalks])
        assertEqual "poor John, walks, quickly"
            [(makeWord "poor John" (Category "n")), tWalks, tQuickly]
            (reducePairInPhrase 0 tPhrase)
        assertEqual "poor, John walks, quickly"
            [tPoor, (makeWord "John walks" (Category "S")), tQuickly]
            (reducePairInPhrase 1 tPhrase)
        assertEqual "poor, John, walks quickly"
            [tPoor, tJohn, (makeWord "walks quickly" (makeBackward "n" "S"))]
            (reducePairInPhrase 2 tPhrase)

tests :: Test
tests = TestList [TestLabel "testCategory"           testCategory,
                  TestLabel "testForward"            testForward,
                  TestLabel "testBackward"           testBackward,
                  TestLabel "testMakeForward"        testMakeForward,
                  TestLabel "testMakeBackward"       testMakeBackward,
                  TestLabel "testNestedMixed"        testNestedMixed,
                  TestLabel "testNestedDeep"         testNestedDeep,
                  TestLabel "testWord"               testWord,
                  TestLabel "testIsForward"          testIsForward,
                  TestLabel "testIsBackward"         testIsBackward,
                  TestLabel "testGetLeftCat"         testGetLeftCat,
                  TestLabel "testGetRightCat"        testGetRightCat,
                  TestLabel "testCanReduceLeft"      testCanReduceLeft,
                  TestLabel "testCanReduceRight"     testCanReduceRight,
                  TestLabel "testCanReducePair"      testCanReducePair,
                  TestLabel "testCanReduceAdjacent"  testCanReduceAdjacent,
                  TestLabel "testBranchingPoints"    testBranchingPoints,
                  TestLabel "testNextBranch"         testNextBranch,
                  TestLabel "testConcatText"         testConcatText,
                  TestLabel "testReducePair"         testReducePair,
                  TestLabel "testReducePairInPhrase" testReducePairInPhrase]
