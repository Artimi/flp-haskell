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

variables :: Formula -> [String]
variables formula = nub $ map (dropWhile (== '-')) $ concat formula

{-getTruthTable :: [[String]] -> String-}
{-getTruthTable formula = show getVariables formula-}

processCommand :: String -> Formula -> String
processCommand command formula
    | command == "-i" = show formula
    | command == "-v" = show $ variables formula
    {-| command == "-t" = getTruthTable formula-}
    | otherwise = "Unknown command: " ++ command

main = do
    [command, file] <- getArgs
    formula <- getFormula file
    let result = processCommand command formula
    putStrLn result
