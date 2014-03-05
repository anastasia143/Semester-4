data Tree a = Nil
             | Node (Tree a) a (Tree a)
    deriving (Show)

maxLen :: Tree a -> Int
maxLen Nil = 0
maxLen (Node leftSubtree a rightSubtree) = 1 + max (maxLen leftSubtree) (maxLen rightSubtree)

minLen :: Tree a -> Int
minLen Nil = 0
minLen (Node leftSubtree a rightSubtree) = 1 + min (minLen leftSubtree) (minLen rightSubtree)

main = do
        let n = Node 
                    (Node
                         (Node 
                              Nil
                         3 
                              (Node
                                   Nil
                              3
                                   Nil))
                     5 
                          (Node
                               Nil 
                          3 
                               (Node
                                   Nil 
                                3 
                                   (Node 
                                       Nil 
                                    3
                                        Nil))))
                 1
                    (Node 
                          Nil 
                    3
                          Nil)
        putStrLn "Max len from root to leaves: "
        putStrLn (show $ maxLen n)
        putStrLn "Min len from root to leaves: "
        putStrLn (show $ minLen n)