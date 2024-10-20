double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns
average2 ns = div (sum ns) (length ns)

fun = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

new_last xs = (drop (length xs - 1) xs) !! 0

add' :: Int -> (Int -> Int)
add' x y = x + y

increment :: Int -> (Int -> Int)
increment x = add' 1

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1/n

abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n = if n < 0 then -1 else
            if n == 0 then 0 else 1

abs2 :: Int -> Int
abs2 n | n >= 0    = n
       | otherwise = -n

not :: Bool -> Bool
not False = True
not True = False

fst2 :: (x, y) -> x
fst2 (x, _) = x

test :: [Char] -> Bool
test ['a', _, _] = True
test _           = False

test_for_a :: [Char] -> Bool -- This function tests if the list of any size has its first
    -- character as 'a'
test_for_a ('a': _) = True 
test_for_a _        = False

add2 :: Int -> (Int -> Int) -- lambda function here
add2 = \x -> (\y -> x + y)

const :: a -> (b -> a)
const x = \_ -> x

odds1 :: Int -> [Int]
odds1 n = map f [0..n]
            where f x = 2*x + 1

odds2 :: Int -> [Int]
odds2 n = map (\x -> 2*x + 1) [0..n]

halve :: [a] -> ([a], [a])
halve l | Main.even(length l) == True = ((take ((length l) `div` 2) l), (drop ((length l) `div` 2) l)) 

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']


-- For strings:
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x, y) | x <- [0..n], y <- [0..m]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

replicate :: Int -> a -> [a]
replicate n a = [ a | i <- [1..n]]

fac :: Int -> Int
fac n = product [1..n]

recfac :: Int -> Int
recfac 0 = 1
recfac n = n * recfac (n-1)

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : (y:ys)
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)