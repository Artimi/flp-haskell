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

    {-withFile file ReadMode (\ handle -> do-}
    {-contents <- hGetContents handle-}
    {-formula <- map (splitOn ',') (lines contents)-}
    {-return formula-}
    {-)-}

    {-handle <- openFile file ReadMode-}
    {-formula <-gf handle []-}
    {-hClose handle-}
    {-return formula-}
    {-where-}
        {-gf handle l = do-}
            {-eof <- hIsEOF handle-}
            {-if eof-}
                {-then return l-}
                {-else do-}
                    {-line <- hGetLine handle-}
                    {-gf handle (l ++ (splitOn ',' line))-}

main = do
    args <- getArgs
    let comm = head args
        file = args !! 1
    formula <- getFormula file
    print formula
