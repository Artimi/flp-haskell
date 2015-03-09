-- project: formula-2-bdd
-- author: Petr Šebek
-- login: xsebek02

import System.IO
import System.Environment
import Data.List

-- | split String when satisfied predicate
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s = case dropWhile p s of
    "" -> []
    s' -> w: splitWhen p s''
        where (w, s'') = break p s'

-- | split String on d Char
splitOn :: Char -> String -> [String]
splitOn d = splitWhen ( == d)

--------------------------------------------------------------------------------
-- | type declaration for Formula
type Formula = [[String]]

-- | read formula from file
getFormula :: FilePath -> IO Formula
getFormula file = do
    contents <- readFile file
    let formula = map (splitOn ',') (lines contents)
    return formula

-- | get unique non-negative variables from formula
getVariables :: Formula -> [String]
getVariables formula = nub $ map (dropWhile (== '-')) $ concat formula

-- | whether clause is satisfied with given values
isSatisfied :: [String] -> [Bool] -> [String] -> Bool
isSatisfied variables values clause = all (`elem` valuesClause) clause
    where valuesClause = map (\ x -> if fst x == False then "-" ++ snd x else snd x) (zip values variables)

--------------------------------------------------------------------------------
type TruthTable = [[Bool]]

-- | retun String representing table
showTable :: TruthTable-> String
showTable table = unlines [ unwords [ show $ fromEnum x | x <- xs] | xs <- table ]

-- | creates truth table according to formula
getTruthTable :: Formula -> TruthTable
getTruthTable formula =
    let variables = getVariables formula
        truthValues = sequence $ replicate (length variables) [False, True]
    in [values ++ [ any (isSatisfied variables values) formula] | values <- truthValues ]


--------------------------------------------------------------------------------
-- | data structure for Binary decision diagram
data BDD a = Leaf Bool | Node a (BDD a) (BDD a) deriving (Show, Eq)

-- | Set leaf under path [Bool] of bdd to new_value
setLeaf :: [Bool] -> BDD a -> Bool -> BDD a
setLeaf (False:ds) (Node x l r) new_value = Node x (setLeaf ds l new_value) r
setLeaf (True:ds) (Node x l r) new_value = Node x l (setLeaf ds r new_value)
setLeaf [] (Leaf b) new_value = Leaf new_value
setLeaf [] (Node _ _ _) _ = error "You are not in a leaf!"

-- | Get value of leaf under path given by (variable, value)
getLeaf :: [(String, Bool)] -> BDD String -> Bool
getLeaf (d:ds) (Node x l r) = if (fst d) == x
    then if (snd d) == False
        then getLeaf ds l
        else getLeaf ds r
    else getLeaf ds (Node x l r)
getLeaf (_:_) (Leaf b) = b
getLeaf [] (Leaf b) = b
getLeaf [] (Node _ _ _) = error "You are not in a leaf!"

-- | getValue from Node or Leaf
getValue :: BDD String -> String
getValue (Node x _ _) = x
getValue (Leaf b) = show $ fromEnum b

-- | Get paths of BDD to leafs
getPaths :: BDD String -> String -> String
getPaths (Node x l r) prefix = (let prefixl = prefix ++ x ++ "->"; prefixr = prefix ++ x ++ "=>" in  (getPaths l prefixl) ++ (getPaths r prefixr))
getPaths (Leaf b) prefix = prefix ++ (show $ fromEnum b) ++ "\n"

-- | return representation of BDD, if empty return only bool value
showBDD :: BDD String -> String
showBDD bdd = getPaths bdd ""

-- | Tests whether BDD is equivalent to truthtables in order of variables
equivalent :: BDD String -> TruthTable -> [String] -> Bool
equivalent  bdd tt variables = all (==True) [(last x) == (getLeaf (zip variables (init x)) bdd) | x <-tt]

-- | Creates BDD from list of variables with leaves equal empty
buildBDD :: [String] -> BDD String
buildBDD (x:[]) = Node x (Leaf False) (Leaf False)
buildBDD (x:xs) = Node x (buildBDD xs) (buildBDD xs)

-- | Set BDD leaves value proper according to truthTable
evaluateBDD :: BDD a -> TruthTable -> BDD a
evaluateBDD bdd truthTable = foldl (\t values -> setLeaf (init values) t (last values)) bdd truthTable

-- | Return BDD made from formula
getBDD :: Formula -> BDD String
getBDD formula =
    let variables = getVariables formula
        bdd = buildBDD variables
        truthTable = getTruthTable formula
    in evaluateBDD bdd truthTable

-- | Reduce Ordered BDD to ROBDD
reduceBDD :: (Eq a) => BDD a -> BDD a
reduceBDD (Node x l r)
    | l == r = reduceBDD l
    | l /= r = (Node x (reduceBDD l) (reduceBDD r))
reduceBDD self@(Leaf b) = self

-- | check formula correspondence to BDD and ROBDD
check :: Formula -> Bool
check formula = equivalent bdd tt variables && equivalent rbdd tt variables
    where bdd = getBDD formula
          rbdd = reduceBDD bdd
          tt = getTruthTable formula
          variables = getVariables formula

-- | Apply operation op on two bdds
-- | assume variables are in alphabet order
apply :: (Bool -> Bool -> Bool) -> BDD String -> BDD String -> BDD String
apply op b1 b2 = reduceBDD $ applyFrom op b1 b2

applyFrom :: (Bool -> Bool -> Bool) -> BDD String -> BDD String -> BDD String
applyFrom op (Leaf b1) (Leaf b2) = (Leaf $ op b1 b2)
applyFrom op n1@(Node x1 l1 r1) n2@(Leaf b2) = (Node x1 (applyFrom op l1 n2) (applyFrom op r1 n2))
applyFrom op n1@(Leaf b1) n2@(Node x2 l2 r2) = (Node x2 (applyFrom op n1 l2) (applyFrom op n1 r2))
applyFrom op n1@(Node x1 l1 r1) n2@(Node x2 l2 r2) = case compare x1 x2 of
    EQ -> (Node x1 (applyFrom op l1 l2) (applyFrom op r1 r2))
    LT -> (Node x1 (applyFrom op l1 n2) (applyFrom op r1 n2))
    GT -> (Node x2 (applyFrom op n1 l2) (applyFrom op n1 r2))

--------------------------------------------------------------------------------
-- | runs command on formula
processCommand :: String -> Formula -> String
processCommand command formula
    | command == "-i" = (show formula) ++ "\n"
    | command == "-v" = (show $ getVariables formula) ++ "\n"
    | command == "-b" = showBDD $ getBDD formula
    | command == "-t" = unwords (getVariables formula) ++ "\n" ++ (showTable $ getTruthTable formula)
    | command == "-r" = showBDD $  reduceBDD $ getBDD formula
    | command == "-c" = (show $ check formula) ++ "\n"
    | otherwise = "Unknown command: " ++ command

processApply :: String -> Formula -> Formula -> String
processApply operation f1 f2 = let
                        b1 = (reduceBDD $ getBDD f1)
                        b2 = (reduceBDD $ getBDD f2)
                    in case operation of
                                    "||" -> showBDD $ apply (||) b1 b2
                                    "&&" -> showBDD $ apply (&&) b1 b2
                                    op -> error "Unknown operation: " ++ op ++ "\n"

help :: String
help = " ./formula-2-bdd [-i | -v | -b | -t | -r | -c] DNF_FILE\n \
\ \n \
\ -i - print DNF representation\n \
\ -t - print truth table\n \
\ -b - print BDD\n \
\ -r - print ROBDD\n \
\ \n \
\ extra options:\n \
\ -v - print variables used in formula\n \
\ -c - check whether generated BDD and ROBDD correspond to truth table\n \
\ \n \
\ apply function:\n \
\ ./formula-2-bdd OPERATION DNF_FILE1 DNF_FILE2\n \
\ \n \
\ Run apply with OPERATION = [\"||\",\"&&\"] on ROBDDs from DNF_FILE1 DNF_FILE2\n"

main = do
    args <- getArgs
    case args of
        [command, file] -> do
                        formula <- getFormula file
                        let result = processCommand command formula
                        putStr result
        [operation, file1, file2] -> do
                                formula1 <- getFormula file1
                                formula2 <- getFormula file2
                                let result = processApply operation formula1 formula2
                                putStr result
        _ -> putStr help
