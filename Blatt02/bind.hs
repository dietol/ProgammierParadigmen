f y = \z -> x + 7 * z - y
x = 1
g x = x + (let y = x * 2; x = 5 * 5 in (let w = f x 2 in w + y))
h = let z = 2 in g x + (\z -> -z) z where z = 3

