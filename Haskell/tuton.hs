doubleMe x = x + x

doubleSmallNumber x = if x > 100
			then x
			else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

doubleUs x y = x*2 + y*2

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int 
addThree x y z = x + y + z 

factorial n = product [1..n] 

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"  

head' :: [a] -> a
head' [] = error "Hey, you're stupid!"
head' (x:xs) = x

tell :: (Show a) => [a] -> String
tell [] = "no elements!"
tell (x:[]) = "just one element: " ++ show x  
tell (x:y:[]) = "here are two objects: " ++ show x ++ ", " ++ show y
tell (x:xs) = "here is the damn list: " ++ show (x:xs)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + length' xs

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

