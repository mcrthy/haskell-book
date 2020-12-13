module ChapterExercises where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup k' [] = Nothing
myLookup k' ((k, v):kvs)
  | k' == k   = Just v
  | otherwise = myLookup k' kvs

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f p = f (fst p) (snd p)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' d Nothing = d
fromMaybe' _ (Just v) = v

xs :: Maybe Integer
xs = myLookup 3 $ zip x y

ys :: Maybe Integer
ys = myLookup 6 $ zip y z

zs :: Maybe Integer
zs = myLookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = myLookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer
   -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed p = uncurry' (+) p

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = fmap summed (liftA2 (,) xs ys)

main :: IO ()
main = do
  print $ foldr (&&) True $ sequA 7
  print $ sequA (fromMaybe 0 s')
  print $ bolt (fromMaybe 0 ys)