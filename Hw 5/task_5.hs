import Control.Monad

elemWithSmallNeighbors :: [Int] -> Maybe Int
elemWithSmallNeighbors [] = Nothing
elemWithSmallNeighbors [x] = Nothing
-- zip3 (x:y:zs) (y:zs) zs - ��������� ���� ������ � ���� �� 3, ����� ���������� ������� ������� � ��� �������
-- liftM comparison - ������ ��������� � ������ ������; return, ���� ����� ������ �������
-- foldr mplus Nothing - ������ �������, ����� ���������� �� ������, � ���� �������� (mplus x _ = x � Maybe)
elemWithSmallNeighbors (x:y:zs) = foldr mplus Nothing (liftM comparison $ zip3 (x:y:zs) (y:zs) zs)
                                  where comparison (a,b,c) = if (a < b) && (b > c) then Just b else Nothing

main = do
    putStrLn(show $ elemWithSmallNeighbors [1,2,3,3,7,5,8,3])
    putStrLn(show $ elemWithSmallNeighbors [1,2,3,3,4,5,8,9])
    putStrLn(show $ elemWithSmallNeighbors [3,4])
    putStrLn(show $ elemWithSmallNeighbors [3,4,1])
    putStrLn(show $ elemWithSmallNeighbors [1,2,1000,3,4,5,3848,9])