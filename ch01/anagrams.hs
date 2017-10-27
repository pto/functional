select :: (a -> Bool) -> [a] -> [a]

isLength :: Int -> Word -> Bool

combinations :: Word -> [(Word, Word)]

sortPairs :: [(a, a)] -> [(a, a)]

group :: [(Word, Word)] -> [(Word, [Word])]

showEntry :: (Word, [Word]) -> String

heading :: Int -> String

anagrams :: Int -> [Word] -> String
anagrams n words = heading n ++ 
                   (concat . map showEntry . group . sortPairs . concat . 
                   map combinations . (select (isLength n))) words
