import Data.Char

-- letter to int function:
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let x = chr (ord 'a' + x)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

caesarencoding :: Int -> String -> String
caesarencoding n xs = [shift n x | x <- xs]

-- To encode, use positive shift factor
-- To decode, use negative shift factor
-- on the encrypted string


lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs