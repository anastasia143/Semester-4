data Polynomial a = Polynomial [(a, Int)]

fromPolToList :: Polynomial a -> [(a, Int)]
fromPolToList (Polynomial pair) = pair

-- FOR: ax + bx = (a+b)x
addHelper x y = ((fst x) + (fst y), snd y)
coefAdd :: (Num a, Ord a, Show a) => [(a, Int)] -> (a, Int) -> [(a, Int)]
coefAdd xs val = helper xs val [] False
        where helper xs val newxs True = xs ++ newxs
              helper [] val newxs False = val:newxs
              helper (x:xs) val newxs False | (snd x == snd val) = (helper xs val ((addHelper x val):newxs) True)
                                            | otherwise =  helper xs val (x:newxs) False

-- FOR: (ax^2 + bx) x^3 = ax^5 + bx^4
multHelper x y = ((fst x) * (fst y), (snd x) + (snd y))
polOnMonoMult :: (Num a, Ord a, Show a) => [(a, Int)] -> (a, Int) -> [(a, Int)]
polOnMonoMult xs val = helper xs val []
       where helper [] val newxs = newxs
             helper (x:xs) val newxs = helper xs val ((multHelper x val):newxs)

-- INTERFACE: simplify, add, multiply, show

instance (Num a, Ord a, Show a) => Show (Polynomial a) where
    show (Polynomial []) = show 0
    show (Polynomial [x]) = show(fst x) ++ "*" ++ showDeg(snd x)
             where showDeg 0 = ""
                   showDeg 1 = "x"
                   showDeg d = "x^" ++ show d
    show (Polynomial (x:xs)) = show(fst x) ++ "*" ++ showDeg(snd x) ++ " + " ++ show (Polynomial xs)
             where showDeg 0 = ""
                   showDeg 1 = "x"
                   showDeg d = "x^" ++ show d

simplify :: (Num a, Ord a, Show a) => Polynomial a -> Polynomial a
simplify (Polynomial xs) = Polynomial (helper [] (fromPolToList $ Polynomial xs))
      where helper newPol [] = newPol
            helper newPol (x:xs) = helper (coefAdd newPol x) xs
            
add :: (Num a, Ord a, Show a) => Polynomial a -> Polynomial a -> Polynomial a 
add pol1 pol2 = simplify $ Polynomial ((helper (fromPolToList pol1) (fromPolToList pol2)))
     where helper [] p2 = p2
           helper p1 [] = p1
           helper xs (y:ys) = helper (coefAdd xs y) ys
           
multiply :: (Num a, Ord a, Show a) => Polynomial a -> Polynomial a -> Polynomial a 
multiply pol1 pol2 = simplify $ Polynomial(helper (fromPolToList pol1) (fromPolToList pol2))
     where helper [] p2 = []
           helper p1 [] = []
           helper xs (y:ys) = fromPolToList $ add (Polynomial (polOnMonoMult xs y)) (Polynomial (helper xs ys))                  

main = do
    let p = Polynomial[(3,4),(2,1),(2,4)]
    putStr "p1 = "
    putStrLn(show $ simplify p)
    let p2 = Polynomial[(3,4),(2,2)]
    putStr "p2 = "
    putStrLn(show $ simplify p2)
    putStr "p1 + p2 = "
    putStrLn(show $ add p p2)
    putStr "p1 * p2 = "
    putStrLn(show $ multiply p p2)
