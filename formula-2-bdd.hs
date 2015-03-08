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

-- | convert True to 1, False to 0
boolToInt :: Bool -> Int
boolToInt bool = if bool then 1 else 0

intToBool :: Int -> Bool
intToBool int = if int == 0 then False else True

type TruthTable = [[Bool]]

-- | retun String representing table
showTable :: TruthTable-> String
showTable table = unlines [ unwords [ show $ boolToInt x | x <- xs] | xs <- table ]

-- | creates truth table according to formula
getTruthTable :: Formula -> TruthTable
getTruthTable formula =
    let variables = getVariables formula
        truthValues = sequence $ replicate (length variables) [False, True]
    in [values ++ [ any (isSatisfied variables values) formula] | values <- truthValues ]

-- | check formula
check :: Formula -> Bool
check formula = equivalent bdd tt variables && equivalent rbdd tt variables
    where bdd = getBDD formula
          rbdd = reduceBDD bdd
          tt = getTruthTable formula
          variables = getVariables formula

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


-- | data structure for Binary decision diagram
data BDD a = Leaf Bool | Node a (BDD a) (BDD a) deriving (Show, Eq)

-- | Reduce Ordered BDD to ROBDD
reduceBDD :: (Eq a) => BDD a -> BDD a
reduceBDD (Node x l r)
    | l == r = reduceBDD l
    | l /= r = (Node x (reduceBDD l) (reduceBDD r))
reduceBDD self@(Leaf b) = self

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

-- | getValue from Node or Leaf
getValue :: BDD String -> String
getValue (Node x _ _) = x
getValue (Leaf b) = show $ boolToInt b

-- | return representation of BDD, if empty return only bool value
showBDD :: BDD String -> String
showBDD (Node x l r) = showBDD' (Node x l r)
showBDD (Leaf b) = showBDD'' (Leaf b)

showBDD' (Node x l r) = x ++ "->" ++ (getValue l) ++ "\n" ++ x ++ "=>" ++ (getValue r) ++ "\n" ++ (showBDD' l) ++ (showBDD' r)
showBDD' (Leaf b) = ""

showBDD'' (Leaf b) = (show $ boolToInt b) ++ "\n"

main = do
    [command, file] <- getArgs
    formula <- getFormula file
    let result = processCommand command formula
    putStr result
