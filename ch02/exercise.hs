import Data.Char

modernize :: String -> String
modernize = unwords . map capitalize . words

capitalize :: String -> String
capitalize xs = toUpper (head xs) : tail xs

first :: (a -> Bool) -> [a] -> a
first p xs | null xs   = error "Empty list"
           | p x       = x
           | otherwise = first p (tail xs)
           where x = head xs

first' :: (a -> Bool) -> [a] -> Maybe a
first' p xs | null xs   = Nothing
            | p x       = Just x
            | otherwise = first' p (tail xs)
            where x = head xs

f :: Maybe a -> Maybe a
f _ = Nothing

g :: Maybe a -> Maybe a
g x = x

h :: Maybe a -> Maybe a
h _ = undefined

i :: Maybe a -> Maybe a
i Nothing = undefined
i x       = x

j :: Maybe a -> Maybe a
j Nothing = Nothing
j _       = undefined

k :: Maybe a -> Maybe a
k Nothing = undefined
k _       = Nothing

exp' :: Integer -> Integer -> Integer
exp' x n | n == 0  = 1
         | n == 1  = x
         | even n  = let half = exp' x (n `div` 2) in half * half
         | odd n   = let half = exp' x (n `div` 2) in half * half * x

-- exp' 2 10 has 2^3 <= n < 2^4
-- exp' 2 10 = exp' 2 5 * itself
-- exp' 2 5  = exp' 2 2 * itself * x
-- exp' 2 2  = exp' 2 1 * itself
-- 4 multiplications, so p + 1?
-- 11,5(2),2(2),1(1)
-- 12,6(1),3(1),1(2)
-- p * 2, worst case?
-- 1, 3, 7, 15, 31
-- exp 2 31 = 2 * (exp (2 * 2) 14)
--          = 2 * (exp (
--
exp'' x n | n == 0  = 1
          | n == 1  = x
          | even n  = exp'' (x*x) m
          | odd n   = x*exp'' (x*x) m
          where m = n `div` 2
