insert n [] = [n]
insert n (x:xs) = insertRek n [] (x:xs)

insertRek n (x:xs) (y:ys)
	| (y<n) = insertRek n (x:xs ++ [y]) ys
	| (y>=n) =  x:xs ++ n:y:ys 

insertRek n [] (y:ys)
	| (y<n) = insertRek n [y] ys
        | (y>=n) =  n:y:ys

insertRek n (x:xs) [] = x:xs ++ [n]
