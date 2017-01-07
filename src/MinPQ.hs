module MinPQ (
  MinPQ,
  empty,
  insert,
  delMin
) where

data Heap a =
  Node { left :: Heap a  -- ^ The left subtree
       , right :: Heap a -- ^ The right subtree
       , val :: a        -- ^ The value stored at the current node
       , size :: Int     -- ^ The size (number of elements) of the current node.
       }
  | Leaf                 -- ^ The empty tree.
  deriving (Show, Eq)

type MinPQ a = Heap a

empty :: Heap a
empty = Leaf

insert :: (Ord a) => a -> Heap a -> Heap a
insert x heap = merge heap (Node Leaf Leaf x 1)

delMin :: (Ord a) => Heap a -> Maybe (a, Heap a)
delMin Leaf = Nothing
delMin (Node l r v s) = Just (v, merge l r)

swap :: Eq a => Heap a -> Heap a
swap h@(Node l r v s)
  | (l == Leaf && r /= Leaf) || (r /= Leaf && size l < size r) = Node r l v s
  | otherwise       = h

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge a Leaf = a
merge Leaf b = b
merge h1@(Node la ra va sa) h2@(Node lb rb vb sb)
  | va < vb   = swap $ Node la (merge h2 ra) va (sa + sb)
  | otherwise = swap $ Node lb (merge h1 rb) vb (sa + sb)
