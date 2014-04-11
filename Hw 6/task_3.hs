data BinarySearchTree a = Nil | Node (BinarySearchTree a) a (BinarySearchTree a)
                         deriving (Eq)

add :: (Ord a) => BinarySearchTree a -> a -> BinarySearchTree a
add Nil n = Node Nil n Nil
add (Node leftSubtree n rightSubtree) x | x < n = Node (add leftSubtree x) n rightSubtree
                                        | x > n = Node leftSubtree n (add rightSubtree x)
                                        | otherwise = Node leftSubtree x rightSubtree

remove :: (Ord a) => BinarySearchTree a -> a -> BinarySearchTree a
remove Nil _ = Nil
remove (Node leftSubtree n rightSubtree) x | x < n = Node (remove leftSubtree x) n rightSubtree
                                           | x > n = Node leftSubtree n (remove rightSubtree x)
                                           | otherwise = helper
         where helper | rightSubtree == Nil = leftSubtree
                      | otherwise = Node leftSubtree (minChild rightSubtree) (remove rightSubtree (minChild rightSubtree))
                      where minChild (Node leftSubtree n rightSubtree) | leftSubtree == Nil = n
                                                                       | otherwise = minChild leftSubtree
search :: (Ord a) => BinarySearchTree a -> a -> Bool
search Nil _ = False
search (Node leftSubtree n rightSubtree) x | x < n = search leftSubtree x
                                           | x > n = search rightSubtree x
                                           | otherwise = True
                                         
size :: BinarySearchTree a -> Int
size Nil = 0
size (Node leftSubtree _ rightSubtree) = 1 + (size leftSubtree) + (size rightSubtree)

height :: BinarySearchTree a -> Int
height Nil = 0
height (Node leftSubtree _ rightSubtree) = 1 + max (height leftSubtree) (height rightSubtree)

main = do
     let n = Nil
     putStrLn(show $ search n 2)
     let n2 = add (add (add (add (add n 4) 6) 2) 10) 100
     putStrLn(show $ search n2 2)
     let n3 = remove n2 2
     putStrLn(show $ search n3 2)
     putStrLn(show $ size n2)
     putStrLn(show $ height n2)
