intersect :: (Ord t) => [t] -> [t] -> [t]
intersect xs ys = filter (`isInSec` ys) xs
		where isInSec x zs= x `elem` zs
