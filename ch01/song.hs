import Data.Char

song :: Int -> String
song n  = if n == 0 then ""
          else song (n - 1) ++ "\n" ++ verse n

verse :: Int -> String
verse n = line1 n ++ line2 n ++ line3 n ++ line4 n

units :: [String]
units = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven",
         "Eight", "Nine"]

line1 :: Int -> String
line1 n = units !! n ++ " " ++ (if n == 1 then "man" else "men") ++
          " went to mow\n"

line2 :: Int -> String
line2 _ = "Went to mow a meadow\n"

line3 :: Int -> String
line3 n | n == 1    = "One man and his dog\n"
        | otherwise = units !! n ++ " men, " ++ (map toLower (line3 (n - 1)))

line4 :: Int -> String
line4 _ = "Went to mow a meadow\n"
