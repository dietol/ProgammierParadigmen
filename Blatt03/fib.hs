fibs :: [Integer]
fibs = [fibRek x |x <- [0..]]
	where fibRek x 
		| x<0 = error "No valid Number"
		| x==0 = 0
		| x==1 = 1
		| otherwise = fibRek (x-1) + fibRek (x-2)


collatz :: Int -> [Int]
collatz x = iterate col x
		where col y = if even y then div y 2 else 3*y+1

num :: Int -> Int
num x = length $ takeWhile (>1) $ collatz x


maxNum :: Int -> Int -> (Int, Int)
maxNum a b = foldl (\(acc, accN) x -> if (num x) > accN then (x, num x) else (acc, accN)) (a, num a) [a..b]
