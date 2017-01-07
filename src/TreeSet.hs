module TreeSet(
  empty,
  insert,
  contains
  ) where

data TreeSet a = Node {
    val :: a,
    left :: TreeSet a,
    right :: TreeSet a
  } | Leaf

empty :: TreeSet a
empty = Leaf

insert :: (Ord a) => a -> TreeSet a -> TreeSet a
insert a Leaf = Node a Leaf Leaf
insert a n@(Node v l r)
  | a < v     = (Node v (insert a l) r)
  | a > v     = (Node v l (insert a r))
  | otherwise = n

contains :: (Ord a) => TreeSet a -> a -> Bool
contains Leaf a = False
contains (Node v l r) a
  | a == v = True
  | a <  v = contains l a
  | a >  v = contains r a
