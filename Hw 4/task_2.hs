data Tree a = Nil
             | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

fromTreeToList :: Tree a -> [a]
fromTreeToList = helper []
      where helper xs Nil = xs
            helper xs (Node leftSubtree a rightSubtree) = helper (a:(helper xs leftSubtree)) rightSubtree
            
--Just definition of foldl with tree instead of list
treeFoldl :: (a -> b -> a) -> a -> Tree b -> a
treeFold func start Nil = 0
treeFoldl func start = foldl func start . fromTreeToList
    
main = do
        let n = Node 
                    (Node
                         (Node 
                              Nil
                         1 
                              (Node
                                   Nil
                              5
                                   Nil))
                     6
                          (Node
                               Nil 
                          3 
                               (Node
                                   Nil 
                                4 
                                   (Node 
                                       Nil 
                                    1
                                        Nil))))
                 1
                    (Node 
                          Nil 
                    2
                          Nil)
        putStrLn "Convolution with addition of tree nodes:" 
        putStrLn (show $ treeFoldl (+) 0 n)
        putStrLn (show $ treeFoldl (+) 0 Nil)