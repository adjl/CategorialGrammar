module CategorialGrammar where

import Data.List.Split

data Category = Category { getStr  :: String }
              | Forward  { getLeft :: Category, getRight :: Category }
              | Backward { getLeft :: Category, getRight :: Category }
    deriving Eq

instance Show Category where
    show (Category category) = category
    show (Forward left@(Category _) right@(Category _))
        = show left ++ "/" ++ show right
    show (Forward left@(Category _) right)
        = show left ++ "/(" ++ show right ++ ")"
    show (Forward left right@(Category _))
        = "(" ++ show left ++ ")/" ++ show right
    show (Forward left right)
        = "(" ++ show left ++ ")/(" ++ show right ++ ")"
    show (Backward left@(Category _) right@(Category _))
        = show left ++ "\\" ++ show right
    show (Backward left@(Category _) right)
        = show left ++ "\\(" ++ show right ++ ")"
    show (Backward left right@(Category _))
        = "(" ++ show left ++ ")\\" ++ show right
    show (Backward left right)
        = "(" ++ show left ++ ")\\(" ++ show right ++ ")"

makeForward :: String -> String -> Category
makeForward left right = Forward (Category left) (Category right)

makeBackward :: String -> String -> Category
makeBackward left right = Backward (Category left) (Category right)

----------------------------------------------------------

data Word = Word { getText :: String, getCategory :: Category, getLatex :: String }

instance Eq Word where
    (==) (Word t1 c1 _) (Word t2 c2 _) = t1 == t2 && c1 == c2

instance Show Word where
    show (Word text category _) = "\"" ++ text ++ "\"::" ++ show category

makeWord :: String -> Category -> Word
makeWord text category = Word text category (genBasicLatex text category)

getCatLatex :: Category -> String
getCatLatex category  = foldl1 merge $ splitOn "\\" $ show category
    where merge s1 s2 = s1 ++ "\\textbackslash " ++ s2

genBasicLatex :: String -> Category -> String
genBasicLatex text category = "\\infer{\\text{" ++ text ++ " {::} "
    ++ getCatLatex category ++ "}}{}\n"

isForward :: Word -> Bool
isForward (Word _ (Forward _ _) _) = True
isForward                       _  = False

isBackward :: Word -> Bool
isBackward (Word _ (Backward _ _) _) = True
isBackward                        _  = False

getLeftCat :: Word -> Category
getLeftCat = getLeft . getCategory

getRightCat :: Word -> Category
getRightCat = getRight . getCategory

----------------------------------------------------------

canReduceLeft :: Word -> Word -> Bool
canReduceLeft word1 word2 = isBackward word2 && getLeftCat word2 == getCategory word1

canReduceRight :: Word -> Word -> Bool
canReduceRight word1 word2 = isForward word1 && getRightCat word1 == getCategory word2

canReducePair :: Word -> Word -> Bool
canReducePair word1 word2 = canReduceLeft word1 word2 || canReduceRight word1 word2

canReduceAdjacent :: [Word] -> [Bool]
canReduceAdjacent phrase = zipWith canReducePair phrase (tail phrase)

----------------------------------------------------------

data StackState = StackState { getPhrase :: [Word], getIndices :: [Int] }
    deriving Eq

instance Show StackState where
    show (StackState phrase indices) = "{" ++ show phrase ++ "|" ++ show indices ++ "}"

branchingPoints :: [Word] -> StackState
branchingPoints phrase = StackState phrase (branchIndices (canReduceAdjacent phrase) 0)
    where branchIndices [] _ = []
          branchIndices (isReducible:flags) index
              | isReducible = index : restOfIndices
              | otherwise   =         restOfIndices
              where restOfIndices = branchIndices flags (succ index)

nextBranch :: [StackState] -> (Int, [Word], [StackState])
nextBranch [] = (-1, [], [])
nextBranch (state:states)
    | null $ getIndices state = nextBranch states
    | otherwise               = (index, phrase, newStates)
    where phrase    = getPhrase state
          index     = head $ getIndices state
          indices   = tail $ getIndices state
          newStates = (StackState phrase indices) : states

----------------------------------------------------------

concatText :: Word -> Word -> String
concatText word1 word2 = getText word1 ++ " " ++ getText word2

mkReduced :: Word -> Word -> Category -> String -> Word
mkReduced word1 word2 category dir = Word text category (genNestedLatex text category nestedLatex d)
    where text        = concatText word1 word2
          nestedLatex = getLatex word1 ++ "&&\n" ++ getLatex word2
          d           = if dir == "\\" then "\\textbackslash" else "/"

genNestedLatex :: String -> Category -> String -> String -> String
genNestedLatex text category nested d = "\\infer [\\text{\\scriptsize" ++ d ++ "}] "
    ++ "{\\text{" ++ text ++ " {::} " ++ getCatLatex category ++ "}}{\n"
    ++ nested ++ "}\n"

reducePair :: Word -> Word -> Word
reducePair word1 word2
    | canReduceLeft  word1 word2 = mkReduced word1 word2 (getRightCat word2) "\\"
    | canReduceRight word1 word2 = mkReduced word1 word2 (getLeftCat  word1) "/"

reducePairInPhrase :: Int -> [Word] -> [Word]
reducePairInPhrase index phrase = phraseHead ++ [reducedWord] ++ phraseTail
    where reducedWord = reducePair (phrase !! index) (phrase !! succ index)
          phraseHead  = take index phrase
          phraseTail  = drop (index + 2) phrase

----------------------------------------------------------
-- Lexicon
john    = makeWord "John"    (Category "n")
mary    = makeWord "Mary"    (Category "n")
poor    = makeWord "poor"    (makeForward  "n" "n")
lucky   = makeWord "lucky"   (makeForward  "n" "n")
walks   = makeWord "walks"   (makeBackward "n" "S")
sings   = makeWord "sings"   (makeBackward "n" "S")
dances  = makeWord "dances"  (makeBackward "n" "S")
loves   = makeWord "loves"   (Forward  (makeBackward "n" "S") (Category "n"))
quickly = makeWord "quickly" (Backward (makeBackward "n" "S") (makeBackward "n" "S"))
andW    = makeWord "and"     (Backward (Category "S")         (makeForward  "S" "S"))

irreducible :: [Word]
irreducible = [poor, walks]

reducibleOnce :: [Word]
reducibleOnce = [john, walks]

reducibleTwice :: [Word]
reducibleTwice = [john, walks, quickly]

----------------------------------------------------------

phrase1 :: [Word]
phrase1 = [poor, john, walks, quickly]

phrase2 :: [Word]
phrase2 = [poor, john, loves, lucky, mary]

phrase3 :: [Word]
phrase3 = [lucky, mary, sings, andW, poor, john, dances]

testPhrase :: [Word]
-- testPhrase = irreducible
-- testPhrase = reducibleOnce
-- testPhrase = reducibleTwice
-- testPhrase = phrase1
-- testPhrase = phrase2
testPhrase = phrase3

----------------------------------------------------------

categorialGrammar :: [Word] -> [StackState] -> IO [StackState]
categorialGrammar phrase stack = do
    let branches                                   = branchingPoints phrase
        currentBranch@(index, newPhrase, newStack) = nextBranch (branches : stack)
    if currentBranch == (-1, [], [])
        then return []
        else do
            let reducedPhrase       = reducePairInPhrase index newPhrase
                fullyReduced phrase = length phrase == 1 &&
                    (getStr $ getCategory $ phrase !! 0) == "S"
            if fullyReduced reducedPhrase
                then return $ branchingPoints reducedPhrase : newStack
                else categorialGrammar reducedPhrase newStack

reductionPath :: [StackState] -> IO ()
reductionPath [] = return ()
reductionPath (state:stack) = do
    print $ getPhrase state
    reductionPath stack

main :: IO ()
main = do
    stack <- categorialGrammar testPhrase [] -- Specify phrase to reduce here
    if null stack
        then putStrLn "No reduction tree found."
        else do
            putStrLn "Reduction tree found: "
            reductionPath stack
            let latex = getLatex $ (getPhrase $ stack !! 0) !! 0
            putStr "Writing derivation tree LaTeX code to file ..."
            writeFile "derivationTreeLaTeX.txt" latex

----------------------------------------------------------
-- To run tests:
-- $ ghci
-- Prelude> :l CategorialGrammarTest
-- *Main> runTestTT tests
--
-- To try interactively:
-- Prelude> :l CategorialGrammarTest
-- *Main> branchingPoints reducibleTwice
-- *Main> let (i0, p0, s0) = nextBranch (it : []) -- Empty stack
-- *Main> reducePairInPhrase i0 p0
-- *Main> branchingPoints it
-- *Main> let (i1, p1, s1) = nextBranch (it : s0) -- Updated stack
-- *Main> -- And so on...
--
