module Tree where

data Tree a = Leaf a | Node (Tree a) (Tree a)
instance (Show a) => Show (Tree a) where
  show (Leaf z) = show z
  show (Node a b)  = "[" ++ (show a) ++","++ (show b)++"]"


relabel :: Tree a -> Int -> (Tree  (Int, a), Int)
relabel (Leaf l) x = ( Leaf (x, l), x+1)
relabel (Node l r) i = ((Node  l1 r1 ), i2)
      where
        (l1,i1) = relabel l i
        (r1, i2) = relabel r i1


-- type WithCounter a = Int -> (a, Int)

-- Exercise 1.1
type State s a = s -> (a, s)

type WithCounter a = State Int a

next:: WithCounter a -> (a -> WithCounter b ) -> WithCounter b
f  `next`  g = \x ->  let (ra, ri ) =  f x  in g ra ri


purez:: a -> WithCounter a
purez v = \x -> (v, x)

relabelMTree:: Tree a -> WithCounter (Tree (a,Int) )
relabelMTree (Leaf l) = \x -> (Leaf (l,x), x+1)
relabelMTree (Node l r ) = (relabelMTree l) `next` (\w -> relabelMTree r
                      `next` (\z -> purez (Node w z)) )





testTree = Node (Node  (Leaf "x") (Leaf "y") ) (Leaf "z")