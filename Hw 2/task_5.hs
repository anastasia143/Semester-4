import Data.Char

rev :: [a] -> [a]
rev [] = []
rev xs = foldl (\l x -> x:l) [] xs

palindrom :: [Char] -> Bool
palindrom [] = True
palindrom s = str == rev str
          where str = map toLower (filter isAlpha s)

main = do
     putStrLn("A roza upala na lapu azora")
     putStrLn(show $ palindrom "a roza upala na lapu azora")
     putStrLn("jdvckndfclked")
     putStrLn(show $ palindrom "jdvckndfclked")