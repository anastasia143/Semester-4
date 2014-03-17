import Control.Monad

elemWithSmallNeighbors :: [Int] -> Maybe Int
elemWithSmallNeighbors [] = Nothing
elemWithSmallNeighbors [x] = Nothing
-- zip3 (x:y:zs) (y:zs) zs - glue one list in a pair of 3 for comparison between the current element and its neighbors
-- liftM comparison - comparison in every triplet; return, if necessary element is found
-- foldr mplus Nothing - convolution for return one value (mplus x _ = x in Maybe)
elemWithSmallNeighbors (x:y:zs) = foldr mplus Nothing (liftM comparison $ zip3 (x:y:zs) (y:zs) zs)
                                  where comparison (a,b,c) = if (a < b) && (b > c) then Just b else Nothing

main = do
    putStrLn(show $ elemWithSmallNeighbors [1,2,3,3,7,5,8,3])
    putStrLn(show $ elemWithSmallNeighbors [1,2,3,3,4,5,8,9])
    putStrLn(show $ elemWithSmallNeighbors [3,4])
    putStrLn(show $ elemWithSmallNeighbors [3,4,1])
    putStrLn(show $ elemWithSmallNeighbors [1,2,1000,3,4,5,3848,9])