data Tree a = Nil
             | Node (Tree a) a (Tree a)
    deriving (Eq, Show)
    
search :: (a -> Bool) -> Tree a -> [a]
search cond = helper [] cond
      where helper xs c Nil = xs
            helper xs c (Node leftSubtree a rightSubtree) | c a = helper (a:(helper xs c leftSubtree)) c rightSubtree
                                                          | otherwise = helper (helper xs c leftSubtree) c rightSubtree
    
main = do
        let n = Node 
                    (Node
                         (Node 
                              Nil
                         19 
                              (Node
                                   Nil
                              65
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
                                    10
                                        Nil))))
                 1
                    (Node 
                          Nil 
                    9
                          Nil)
        putStrLn "Search by condition x < 10:" 
        putStrLn (show $ take 10 $ search (< 10) n)