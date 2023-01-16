{-# LANGUAGE DeriveTraversable #-}

module Data.Tree (
  Tree (..),
  insert,
  invert,
  max_,
  min_,
  mkTree,
  remove,
  search,
  toList,
  (+>),
  (<+),
) where

{- | A tree is either nil or a node with a element and two sub-trees. The left tree contains nodes
   | with elements less than its own; the right, greater.
-}
data Tree a
  = Nil
  | Node a (Tree a) (Tree a)
  deriving
    (Eq, Ord, Show, Foldable, Functor, Traversable)

-- | Combine two trees (requires semigroup defined on element type).
instance Semigroup a => Semigroup (Tree a) where
  (<>) Nil Nil = Nil
  (<>) t1 Nil = t1
  (<>) Nil t2 = t2
  (<>) (Node x ta tb) (Node y tc td) =
    Node (x <> y) (ta <> tc) (tb <> td)

-- | Define an empty tree
instance Semigroup a => Monoid (Tree a) where
  mempty = Nil

-- | Define applicative for tree.
instance Applicative Tree where
  -- pure for tree
  pure x = Node x Nil Nil

  -- tree apply
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) ft@(Node f _ _) (Node x t1 t2) =
    Node (f x) (ft <*> t1) (ft <*> t2)

-- | Define bind on tree.
instance Monad Tree where
  (>>=) Nil _ = Nil
  (>>=) (Node x t1 t2) f = go $ f x
    where
      -- Unpack function result to process children
      go Nil = Nil
      go (Node y t1' t2') =
        Node y (t1' `orElse` (t1 >>= f)) (t2' `orElse` (t2 >>= f))
      -- Determine which tree to use: choose first unless nil
      orElse Nil t = t
      orElse t _ = t

-- | Build a tree from a foldable.
mkTree :: Foldable f => Ord a => f a -> Tree a
mkTree =
  foldl (+>) Nil

-- | Insert right infix operator
infixr 5 <+

-- | Insert alias
(<+) :: Ord a => a -> Tree a -> Tree a
(<+) = insert

-- | Insert left infix operator
infixl 5 +>

-- | Insert alias
(+>) :: Ord a => Tree a -> a -> Tree a
(+>) = flip insert

-- | Add an element to a tree
insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = pure x
insert x n@(Node y t1 t2) =
  case compare x y of
    EQ -> n
    GT -> Node y t1 (insert x t2)
    LT -> Node y (insert x t1) t2

-- | Search for the sub-tree with the given root element
search :: Ord a => a -> Tree a -> Tree a
search _ Nil = Nil
search x n@(Node y t1 t2) =
  case compare x y of
    EQ -> n
    LT -> search x t1
    GT -> search x t2

-- | Remove an element from a tree.
remove :: Ord a => a -> Tree a -> Tree a
remove _ Nil = Nil
remove x (Node y t1 t2) =
  case compare x y of
    LT -> Node y (remove x t1) t2
    GT -> Node y t1 (remove x t2)
    EQ -> case min_ t2 of -- find smallest value > x
      (Just z) -> Node z t1 (remove z t2)
      Nothing -> t1

-- | Find the deepest left value of a tree.
min_ :: Ord a => Tree a -> Maybe a
min_ Nil = Nothing
min_ (Node x t1 _) =
  if t1 == Nil
    then Just x
    else min_ t1

-- | Find the deepest right value of a tree.
max_ :: Ord a => Tree a -> Maybe a
max_ Nil = Nothing
max_ (Node x _ t2) =
  if t2 == Nil
    then Just x
    else max_ t2

-- | Invert a tree
invert :: Tree a -> Tree a
invert Nil = Nil
invert (Node x t1 t2) =
  Node x (invert t2) (invert t1)

-- | Create a list from a tree (depth first).
toList :: Tree a -> [a]
toList Nil = []
toList (Node x t1 t2) =
  [x] <> toList t1 <> toList t2
