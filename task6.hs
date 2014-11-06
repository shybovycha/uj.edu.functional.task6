fix f = f (fix f)
fact f n = if n == 0 then 1 else n * (f (n - 1))

my_map :: (a -> b) -> [a] -> [b]
my_map f list = [ f x | x <- list ]

kw = [ x * x | x <- [1..10], even x ]

is_prime n = null [ x | x <- [2..n-1], (n `mod` x) == 0 ]
gen_primes n = [ x | x <- [1..n], is_prime x ]

put_in pos elt list
    | list == [] = [elt]
    | otherwise = (take pos list) ++ [ elt ] ++ (drop pos list)

perm n = if n == 0 then [[]] else [ put_in p n l | l <- perm (n - 1), p <- [0 .. (n - 1)] ]