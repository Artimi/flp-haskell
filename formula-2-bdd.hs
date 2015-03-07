import System.IO
import System.Environment
import Data.List

splitWhen :: (Char -> Bool) -> [Char] -> [[Char]]
splitWhen p s = case dropWhile p s of
    "" -> []
    s' -> w: splitWhen p s''
        where (w, s'') = break p s'

splitOn :: Char -> [Char] -> [[Char]]
splitOn d = splitWhen ( == d)

type Formula = [[String]]

getFormula :: FilePath -> IO Formula
getFormula file = do
    contents <- readFile file
    let formula = map (splitOn ',') (lines contents)
    return formula

getVariables :: Formula -> [String]
getVariables formula = nub $ map (dropWhile (== '-')) $ concat formula

showTable :: [[Int]] -> String
showTable table = unlines [ unwords [ show x | x <- xs] | xs <- table ]

isSatisfied :: [String] -> [Int] -> [String] -> Bool
isSatisfied variables values clause = all (`elem` valuesClause) clause
    where valuesClause = map (\ x -> if fst x == 0 then "-" ++ snd x else snd x) (zip values variables)

boolToInt :: Bool -> Int
boolToInt bool = if bool then 1 else 0

getTruthTable :: Formula -> String
getTruthTable formula =
    let variables = getVariables formula
        truthValues = sequence $ replicate (length variables) [0, 1]
    in unwords variables ++ "\n" ++ showTable [values ++ [(boolToInt $ any (isSatisfied variables values) formula)] | values <-truthValues ]

processCommand :: String -> Formula -> String
processCommand command formula
    | command == "-i" = show formula
    | command == "-v" = show $ getVariables formula
    | command == "-t" = getTruthTable formula
    | otherwise = "Unknown command: " ++ command

main = do
    [command, file] <- getArgs
    formula <- getFormula file
    let result = processCommand command formula
    putStrLn result
