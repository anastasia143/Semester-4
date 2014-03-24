data Print = Book { name :: String,
                    author:: String,
                    price :: Double } |
             Magazine { name :: String,
                        year :: Int,
                        number:: Int,
                        price :: Double }
           deriving (Eq, Show)

getCommonPrice :: [Print] -> Double 
getCommonPrice [] = 0
getCommonPrice (x:xs) = price x + getCommonPrice xs

main = do
    let book1 = Book
               { name = "ddddd",
                 author = "ssd",
                 price = 10 }
    let book2 = Book
               { name = "ddddd",
                 author = "ssd",
                 price = 70 }
    let book3 = Book
               { name = "ddddd",
                 author = "ssd",
                 price = 20 }
    let mag1 = Magazine
               { name = "dddddd",
                 year = 19,
                 number = 6,
                 price = 5 }
    let mag2 = Magazine
               { name = "dddddd",
                 year = 20,
                 number = 6,
                 price = 15 }
    putStrLn(show $ getCommonPrice [book1, mag2, mag1, book3, book2])