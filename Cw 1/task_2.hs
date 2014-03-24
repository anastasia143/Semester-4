data Tree a = Nil
             | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

fromTreeToList :: Ord a => Tree a -> [a]
fromTreeToList = helper []
      where helper xs Nil = xs
            helper xs (Node leftSubtree a rightSubtree) = helper (a:(helper xs leftSubtree)) rightSubtree
            
search :: (Num a, Ord a) => Tree a -> [a]
search = filter (<0) . fromTreeToList

main = do
    let n = Node 
                    (Node
                         (Node 
                              Nil
                         19 
                              (Node
                                   Nil
                              (-65)
                                   Nil))
                     6
                          (Node
                               Nil 
                          3 
                               (Node
                                   Nil 
                                (-4)
                                   (Node 
                                       Nil 
                                    10
                                        Nil))))
                 1
                    (Node 
                          Nil 
                    9
                          Nil)
    putStrLn(show $ search n)