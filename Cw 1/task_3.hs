data Tree a = Nil
             | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

{- -1 <=> there is no this element in the list
 takeWhile :: creates a list from another one, 
 it inspects the original list and takes from it its elements to the moment when the condition fails, then it stops processing -}

position :: Int -> [Int] -> Int
position n xs | takeWhile (/= n) xs == xs = -1
              | otherwise = 1 + (length $ takeWhile (/= n) xs)
              
{- Algorithm:
 If there is no this element in the list => return -1
 Else return length of list part without this element + 1 (it will be position) 
-}
    
main = do
    putStrLn(show $ position 6 [])
    putStrLn(show $ position 100 [1,2,4,7])
    putStrLn(show $ position 7 [1,2,4,7])
    putStrLn(show $ position 12 [1,2,12,4,7,8])