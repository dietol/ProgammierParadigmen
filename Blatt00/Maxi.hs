module Maxi where

max31 x y z = if (x<y) then if (y<z) then z else y else if (x<z) then z else x

max32 x y z 
	| (x<y) && (y<z) = z
	| (x<y) = y
	| otherwise = x

max33 x y z = max x (max y z)

