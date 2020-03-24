module ACkerman where

import Data.Function.Memoize




ackermann :: Int -> Int -> Int
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann 1 n = n + 2
ackermann 2 n = 2 * n + 3
ackermann 3 n = 2 ^ (n + 3) - 3
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))


acks :: [[Int]]
acks = [ [ case (m, n) of
                (0, _) -> n + 1
                (_, 0) -> acks !! (m - 1) !! 1
                (_, _) -> acks !! (m - 1) !! (acks !! m !! (n - 1))
         | n <- [0..] ]
       | m <- [0..] ]

--main :: IO ()
--main = print $ acks !! 3 !! 20




ackermann2 :: Integer -> Integer -> Integer
ackermann2 0 n = n + 1
ackermann2 m 0 = ackermann2 (m - 1) 1
ackermann2 1 n = n + 2
ackermann2 2 n = 2 * n + 3
ackermann2 3 n = 2 ^ (n + 3) - 3
ackermann2 m n = ackermann2 (m - 1) (ackermann2 m (n - 1))


mack::Int->Int->Int

ack :: Int -> Int -> Int
ack 0 n = succ n
ack m 0 = mack (pred m) 1
ack m n = mack (pred m) (mack m (pred n))


ack2 :: Int -> Int -> Int
ack2 0 n = succ n
ack2 m 0 = ack2 (pred m) 1
ack2 m n = ack2 (pred m) (ack2 m (pred n))

mack = memoize2 ack