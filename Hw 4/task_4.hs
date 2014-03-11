import Data.Eq

exist :: (Eq a) => [a] -> a -> Bool
exist [] _ = False
exist (l:ls) x | x == l = True
               | otherwise = exist ls x

checkUnequal :: (Eq a) => [a] -> Bool
checkUnequal = helper []
         where helper _ [] = True
               helper stack (l:ls) | (exist stack l) = False
                                   | otherwise = helper (l:stack) ls

main = do
       putStrLn(show $ checkUnequal [1,2,8,3,2,7,9,2])
       putStrLn(show $ checkUnequal [1,2,8,3,7,9])
       putStrLn(show $ checkUnequal [1,2,3,8,19,123])
       putStrLn(show $ checkUnequal ['1','2','3','t','8','s','t','f'])
       putStrLn(show $ checkUnequal ['1','2','3','t','8','s'])