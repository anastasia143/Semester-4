data Expression a = Constant a |
                    Variable |
                    Degree (Expression a) Int |
                    Addition (Expression a) (Expression a) |
                    Subtraction (Expression a) (Expression a) |
                    Multiplication (Expression a) (Expression a) |
                    Division (Expression a) (Expression a)
                    
derivative (Constant _) = Constant 0
derivative Variable = Constant 1
derivative (Degree x 0) = Constant 1
derivative (Degree Variable n) = (Multiplication (Constant n) (Degree Variable (n-1)))
derivative (Degree x n) = Multiplication (derivative x) (Multiplication (Constant n) (Degree x (n-1))) 
derivative (Addition x y) = Addition (derivative x) (derivative y)
derivative (Subtraction x y) = Subtraction (derivative x) (derivative y)
derivative (Multiplication x y) = Addition (Multiplication (derivative x) y) (Multiplication (derivative y) x)
derivative (Division x y) = Division (Subtraction (Multiplication (derivative x) y) (Multiplication (derivative y) x)) (Degree y 2)

simplify2 (Degree x 1) = x
simplify2 (Addition x (Constant 0)) = x
simplify2 (Addition (Constant 0) x) = x
simplify2 (Subtraction x (Constant 0)) = x
simplify2 (Subtraction (Constant 0) x) =  Multiplication (Constant (-1)) x
simplify2 (Multiplication x (Constant 1)) = x 
simplify2 (Multiplication (Constant 1) x) = x
simplify2 (Multiplication _ (Constant 0)) = Constant 0
simplify2 (Multiplication (Constant 0) _) = Constant 0
simplify2 (Division x (Constant 1)) = x
simplify2 (Division (Constant 1) x) = x
simplify2 (Division (Constant 0) _) = Constant 0 
simplify2 x = x

simplify (Degree x n) = simplify2 (Degree (simplify x) n)
simplify (Addition x y) = simplify2 (Addition (simplify x) (simplify y))
simplify (Subtraction x y) = simplify2 (Subtraction (simplify x) (simplify y))
simplify (Multiplication x y) = simplify2 (Multiplication (simplify x) (simplify y))
simplify (Division x y) = simplify2 (Division (simplify x) (simplify y))
simplify x = x

derivate = simplify . derivative

showWithBrackets x = "(" ++ show x ++ ")"

-- We don`t know how to show this data => we need to determine it
instance Show a => Show (Expression a) where
    show (Constant a) = show a
    show (Variable) = "x"
    show (Degree x n) = showDeg x ++ "^" ++ show n
         where showDeg (Addition x y) = showWithBrackets (Addition x y)
               showDeg (Subtraction x y) = showWithBrackets (Subtraction x y)
               showDeg (Multiplication x y) = showWithBrackets (Multiplication x y)
               showDeg (Division x y) = showWithBrackets (Division x y)
               showDeg (Degree x n) = showWithBrackets (Degree x n)
               showDeg x = show x
    show (Addition x y) = show x ++ " + " ++ show y
    show (Subtraction x y) = show x ++ " - " ++ showSub y
         where showSub (Addition x y) = showWithBrackets (Addition x y)
               showSub (Subtraction x y) = showWithBrackets (Subtraction x y)
               showSub x = show x
    show (Multiplication x y) = showMul x ++ "*" ++ showMul y
         where showMul (Addition x y) = showWithBrackets (Addition x y)
               showMul (Subtraction x y) = showWithBrackets (Subtraction x y)
               showMul (Division x y) = showWithBrackets (Division x y)
               showMul x = show x
    show (Division x y) = showDiv x ++ "/" ++ showDiv y
         where showDiv (Addition x y) = showWithBrackets (Addition x y)
               showDiv (Subtraction x y) = showWithBrackets (Subtraction x y)
               showDiv (Multiplication x y) = showWithBrackets (Multiplication x y)
               showDiv (Division x y) = showWithBrackets (Division x y)
               showDiv x = show x
               
main = do
    let expr = Variable
    putStr "(x)' = "
    putStrLn(show $ derivate expr)
    let expr = Addition (Degree (Variable) 5) (Degree (Variable) 4)
    putStr "(x^5 + x^4)' = "
    putStrLn(show $ derivate expr)
    let expr = Subtraction (Addition (Addition (Degree (Variable) 5) (Degree (Variable) 4)) (Constant 4)) (Degree (Addition (Degree Variable 5) (Degree Variable 3)) 9)
    putStr "(x^5 + x^4 + 4 - (x^5 + x^3)^9)' = "
    putStrLn(show $ derivate expr)
    putStr "(x*(x^2/5))' = "
    let expr = Multiplication Variable (Division (Degree Variable 2) (Constant 5))
    putStrLn(show $ derivate expr)