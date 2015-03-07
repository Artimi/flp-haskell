import System.IO
import System.Environment

splitWhen :: (Char -> Bool) -> [Char] -> [[Char]]
splitWhen p s = case dropWhile p s of
    "" -> []
    s' -> w: splitWhen p s''
        where (w, s'') = break p s'

splitOn :: Char -> [Char] -> [[Char]]
splitOn d = splitWhen ( == d)

getFormula file = do
    contents <- readFile file
    let formula = map (splitOn ',') (lines contents)
    return formula

processCommand :: String -> [[String]] -> String
processCommand command formula
    | command == "-i" = show formula
    | otherwise = "Unknown command: " ++ command

main = do
    [command, file] <- getArgs
    formula <- getFormula file
    let result = processCommand command formula
    putStrLn result
