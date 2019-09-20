module MagicBoxes where

-- Exercise 1.2

(+++) :: [a] -> [a] -> [a]
[] +++  x = x
(h : tail) +++ x = h : (tail +++ x)

