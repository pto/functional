import Data.Char (toLower)
import Data.List (intercalate)

main = interact (anagrams 6 . sort . words)

-- Display anagrams from an input list of words
anagrams :: Int -> [String] -> String
anagrams n ws = heading n ++ (concat . map showEntry . groupAnagrams . 
             sort . makeAnagrams . filterLength n) ws

-- Format display from pair (anagram, wordlist)
showEntry :: (String, [String]) -> String
showEntry (a, ws) = a ++ ": " ++ intercalate ", " ws ++ "\n"

-- Form anagram with letters in ascending alphabetical order
canonicalAnagram :: String -> String
canonicalAnagram = sort . map toLower

-- Form pair (canonical anagram, wordlist)
makeAnagrams :: [String] -> [(String, [String])]
makeAnagrams ws = [(canonicalAnagram w, [w]) | w <- ws]

-- Quicksort
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort lesser ++ [x] ++ sort greater
    where lesser  = [l | l <- xs, l <= x ]
          greater = [g | g <- xs, g > x ]

-- Include only items of length n
filterLength :: Int -> [[a]] -> [[a]]
filterLength n xs = [x | x <- xs, length x == n]

-- Combine pairs with matching canonical anagrams
groupAnagrams :: [(String, [String])] -> [(String, [String])]
groupAnagrams = foldr combine []
    where combine :: Eq a => (a, [a]) -> [(a, [a])] -> [(a, [a])]
          combine left [] = [left]
          combine (la, lws) ((ra, rws):rights)
             | la == ra  = (la, lws ++ rws):rights
             | otherwise = (la, lws):((ra, rws):rights)

-- Heading for display
heading :: Int -> String
heading n = show n ++ "-letter words\n--------------\n"
