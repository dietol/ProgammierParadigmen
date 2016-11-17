import Data.List

atLeastElements :: [Int] -> Int -> Bool
atLeastElements xs n 
	| (length $ filter (>=n) xs) >= n = True 
	| otherwise = False


hIndexCorrect :: ([Int] -> Int) -> [Int] -> Bool
hIndexCorrect hIndex xs
        | hIndex xs > length xs = False
        | hIndex xs < 0 = False
        | otherwise = not getN && atLeastElements xs (hIndex xs)
        where getN = foldl (\acc x -> if (atLeastElements xs x) == True then True else acc ) False [hIndex xs+1..length xs]

test x = [5..0]

hIndex :: [Int] -> Int
hIndex xs =  head $ filter (>0) $ zipWith herbert [1..] $ reverse $ sort xs
	where herbert a b = if b <= a then b else 0


