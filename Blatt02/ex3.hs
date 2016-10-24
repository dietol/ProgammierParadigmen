module Hirsch where

import Data.List

atLeastElements :: [Int] -> Int -> Bool
atLeastElements xs n = length (filter (>=n) xs) >= n

hIndexCorrect :: ([Int] -> Int) -> [Int] -> Bool
hIndexCorrect hIndexTest xs = atLeastElements xs (hIndexTest xs) && not (atLeastElements xs ((hIndexTest xs)+1))

hIndex :: [Int] -> Int
hIndex xs = let list = filter (\(x,y) -> y<=x) $ zipWith (\x y -> (x,y)) [0..] $ reverse $ sort xs
	in if length list == 0 then length xs else fst $ head $ list

test xs = filter (\(x,y) -> y<=x) $ zipWith (\x y -> (x,y)) [0..] $ reverse $ sort xs

hIndex2 xs =  head $ filter (>0) $ zipWith herbert [1..] $ reverse $ sort xs
        where herbert a b = if b <= a then b else 0

hIndex3 l = helper (reverse (sort l)) 0
	where	helper [] acc = acc
		helper (z:ls) acc
			| z > acc = helper ls (acc + 1)
			| otherwise = acc
