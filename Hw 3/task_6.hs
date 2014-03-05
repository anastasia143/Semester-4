pair :: Char -> Char -> Bool
pair '{' '}' = True
pair '[' ']' = True
pair '(' ')' = True
pair '<' '>' = True
pair _ _ = False

checkBrackets :: [Char] -> [Char] -> Bool
checkBrackets [] [] = True
checkBrackets [] _ = False
checkBrackets (x:xs) [] = checkBrackets xs (x:[])
checkBrackets (x:xs) stack | pair (head stack) x = checkBrackets xs (tail stack)
                           | otherwise = checkBrackets xs (x:stack)

check str = checkBrackets (filter(`elem` "()<>{}[]") str) []

main = do
      putStrLn(show $ check "(sacndvlc(m3[dcve[cdvd]]7{evdre}4<ewf3e[c]>743njcdsmkc)023)")
      putStrLn(show $ check "(")
      putStrLn(show $ check "wcd<dcd>)")