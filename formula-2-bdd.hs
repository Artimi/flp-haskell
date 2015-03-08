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
isSatisfied :: [String] -> [Int] -> [String] -> Bool
isSatisfied variables values clause = all (`elem` valuesClause) clause
    where valuesClause = map (\ x -> if fst x == 0 then "-" ++ snd x else snd x) (zip values variables)

-- | convert True to 1, False to 0
boolToInt :: Bool -> Int
boolToInt bool = if bool then 1 else 0

intToBool :: Int -> Bool
intToBool int = if int == 0 then False else True

type TruthTable = [[Int]]

-- | retun String representing table
showTable :: TruthTable-> String
showTable table = unlines [ unwords [ show x | x <- xs] | xs <- table ]

-- | creates truth table according to formula
getTruthTable :: Formula -> TruthTable
getTruthTable formula =
    let variables = getVariables formula
        truthValues = sequence $ replicate (length variables) [0, 1]
    in [values ++ [(boolToInt $ any (isSatisfied variables values) formula)] | values <-truthValues ]

-- | runs command on formula
processCommand :: String -> Formula -> String
processCommand command formula
    | command == "-i" = show formula
    | command == "-v" = show $ getVariables formula
    | command == "-b" = showBDD $ getBDD formula
    | command == "-t" = unwords (getVariables formula) ++ "\n" ++ (showTable $ getTruthTable formula)
    | command == "-r" = showBDD $  reduceBDD $ getBDD formula
    | otherwise = "Unknown command: " ++ command


-- | data structure for Binary decision diagram
data BDD a = Leaf Bool | Node a (BDD a) (BDD a) deriving (Show, Eq)

-- | Reduce Ordered BDD to ROBDD
reduceBDD :: (Eq a) => BDD a -> BDD a
reduceBDD (Node x l r)
    | l == r = reduceBDD l
    | l /= r = (Node x (reduceBDD l) (reduceBDD r))
reduceBDD self@(Leaf b) = self

-- | Set leaf under path [Int] of bdd to new_value
setLeaf :: [Int] -> BDD a -> Bool -> BDD a
setLeaf (0:ds) (Node x l r) new_value = Node x (setLeaf ds l new_value) r
setLeaf (1:ds) (Node x l r) new_value = Node x l (setLeaf ds r new_value)
setLeaf [] (Leaf b) new_value = Leaf new_value
setLeaf [] (Node _ _ _) _ = error "You are not in a leaf!"

-- | Creates BDD from list of variables with leaves equal empty
buildBDD :: [String] -> BDD String
buildBDD (x:[]) = Node x (Leaf False) (Leaf False)
buildBDD (x:xs) = Node x (buildBDD xs) (buildBDD xs)

-- | Set BDD leaves value proper according to truthTable
evaluateBDD :: BDD a -> TruthTable -> BDD a
evaluateBDD bdd truthTable = foldl (\t values -> setLeaf (init values) t (intToBool (last values))) bdd truthTable

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

-- | return representation of BDD
showBDD :: BDD String -> String
showBDD (Node x l r) = x ++ "->" ++ (getValue l) ++ "\n" ++ x ++ "=>" ++ (getValue r) ++ "\n" ++ (showBDD l) ++ (showBDD r)
showBDD (Leaf b) = ""

main = do
    [command, file] <- getArgs
    formula <- getFormula file
    let result = processCommand command formula
    putStrLn result
