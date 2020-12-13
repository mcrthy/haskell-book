module RandomExample where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x ->
      error $
        "intToDie got non 1-6 integer: "
        ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, s3) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

repeat' :: a -> [a]
repeat' x = xs where xs = x:xs

replicateM' :: Applicative m
            => Int -> m a -> m [a]
replicateM' c f =
    loop c
  where
    loop c'
      |  c' <= 0    = pure []
      |  otherwise = liftA2 (:) f (loop (c'-1))

infiniteDie :: State StdGen [Die]
infiniteDie = repeat' <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM' n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0

  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =

          let (die, nextGen) =
                randomR (1, 6) gen
          in go (sum + die)
                (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go n 0 0

  where
    go :: Int -> Int -> Int -> StdGen -> Int
    go lim sum cnt gen
      | sum >= lim = cnt
      | otherwise  =

          let (die, nextGen) =
                randomR (1, 6) gen
          in go lim (sum + die) 
                (cnt + 1) nextGen

rollsCountLogged :: Int
                 -> StdGen
                 -> (Int, [Die])
rollsCountLogged n g = go n 0 0 [] g

  where
    go :: Int -> Int -> Int 
       -> [Die] -> StdGen 
       -> (Int, [Die])
    go lim sum cnt ds gen
      | sum >= lim = (cnt, ds)
      | otherwise =

          let (d, nextGen) = 
                randomR (1, 6) gen
          in go lim (sum + d) 
                    (cnt + 1)
                    ((intToDie d) : ds) 
                    nextGen

fizzBuzz :: Integer -> String
fizzBuzz n
  | (mod n 3) == 0 && (mod n 5) == 0 = "FizzBuzz"
  | (mod n 3) == 0                   = "Fizz"
  | (mod n 5) == 0                   = "Buzz"
  | otherwise                        = show n

main :: IO ()
main = do
  print $ fizzBuzz <$> [1..100]
