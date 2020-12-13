module ShortExercises where

import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = map toUpper . reverse

fmapped :: [Char] -> [Char]
fmapped = fmap (map toUpper) reverse

tupled :: [Char] -> ([Char], [Char])
tupled xs = ("", rev) <*> (cap xs, xs)

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  c <- cap
  r <- rev
  return $ (c, r)