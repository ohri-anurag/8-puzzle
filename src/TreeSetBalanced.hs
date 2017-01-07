module TreeSetBalanced(
  empty,
  insert,
  contains
  ) where

data TreeSet a =
  Node { val :: a            -- ^ The value stored in the current node.
       , left :: TreeSet a   -- ^ The left subtree.
       , right :: TreeSet a  -- ^ The right subtree.
       , h :: Int            -- ^ The height/depth of the tree.
       }
  | Leaf                     -- ^ The empty tree

height :: TreeSet a -> Int
height Leaf = 0
height node = h node

empty :: TreeSet a
empty = Leaf

maxHeight a b = maximum [height a, height b]

ll (Node v (Node lv ll lr lh) r h) =
  let
    r' = Node v lr r (maxHeight lr r)
  in
    Node lv ll r' (maxHeight ll r')

lr (Node v (Node lv ll (Node lrv a b lrh) lh) r h) =
  let
    l' = Node lv ll a (maxHeight ll a)
    r' = Node v b r (maxHeight b r)
  in
    Node lrv l' r' (maxHeight l' r')

rl (Node v l (Node rv (Node rlv a b rlh) rr rh) h) =
  let
    l' = Node v l a (maxHeight l a)
    r' = Node rv b rr (maxHeight b rr)
  in
    Node rlv l' r' (maxHeight l' r')

rr (Node v l (Node rv rl rr rh) h) =
  let
    l' = Node v l rl (maxHeight l rl)
  in
    Node rv l' rr (maxHeight l' rr)
  

balance :: TreeSet a -> TreeSet a
balance t@(Node v l r h)
  | height l - height r > 1 =
    if height (left l) - height (right l) > 1
      then ll t
      else lr t
  | height r - height l > 1 =
    if height (left r) - height (right r) > 1
      then rl t
      else rr t
  | otherwise = t

insert :: (Ord a) => a -> TreeSet a -> TreeSet a
insert a Leaf = Node a Leaf Leaf 1
insert a n@(Node v l r h)
  | a < v     = let l' = insert a l in l' `seq` Node v l' r (maxHeight l' r)
  | otherwise = let r' = insert a r in r' `seq` Node v l r' (maxHeight l r')

contains :: (Ord a) => TreeSet a -> a -> Bool
contains Leaf a = False
contains (Node v l r h) a
  | a <  v = contains l a
  | a >  v = contains r a
  | otherwise = True
