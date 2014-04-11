data Tree a = Nil
            | Node (Tree a) a (Tree a)
    deriving (Eq)
    
treeToString :: Tree Char -> String
treeToString = helper ""
    where helper str Nil = 'e':str
          helper str (Node leftSubtree x rightSubtree) = 'n':x:(helper (helper str leftSubtree) rightSubtree)
          
stringToTree :: String -> Tree Char
stringToTree = fst . parse
    where parse ('e':xs) = (Nil, xs)
          parse ('n':x:xs) = let (leftSubtree, leftRemain) = parse xs in
                                let (rightSubtree, rightRemain) = parse leftRemain in
                                     (Node leftSubtree x rightSubtree, rightRemain)
-- Don't use "where" instead of "let in". Node can't find leftSubtree.                                     
                                     
-- Bypass from the left subtree
printTree :: Tree a -> [a]
printTree = helper []
      where helper xs Nil = xs
            helper xs (Node leftSubtree a rightSubtree) = helper ((helper xs leftSubtree) ++ [a]) rightSubtree
            
main = do
    let tr = Node 
                    (Node
                         (Node 
                              Nil
                         '1' 
                              (Node
                                   Nil
                              '5'
                                   Nil))
                     '6'
                          (Node
                               Nil 
                          '3' 
                               (Node
                                   Nil 
                                '4' 
                                   (Node 
                                       Nil 
                                    '1'
                                        Nil))))
                 '1'
                    (Node 
                          Nil 
                    '2'
                          Nil)
    putStrLn(show $ treeToString tr)
    let tr2 = stringToTree "n1n2een6n3n4n1eeeen1n5eee"
    putStrLn(show $ printTree tr2)