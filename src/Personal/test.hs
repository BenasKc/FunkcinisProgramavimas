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
