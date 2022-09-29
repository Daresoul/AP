module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West
  deriving (Eq, Show, Read, Ord)

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move West (x,y) = (x-1, y)
move East (x,y) = (x+1, y)

moves :: [Direction] -> Pos -> Pos
moves (x:xs) pos = if length xs > 0 then moves xs (move x pos)
                                    else move x pos
moves [] pos = pos

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add Zero y = y
add x Zero = x
add x (Succ y) = add (Succ x) y

mult :: Nat -> Nat -> Nat
mult Zero y = Zero
mult x Zero = Zero
mult x (Succ y) = add (mult x y) x

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = nat2int x + 1

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ (int2nat (x - 1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node y t1 t2)
  | x > y = Node y t1 (insert x t2)
  | x < y = Node y (insert x t1) t2
  | otherwise = Node y t1 t2

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Eq, Show, Read, Ord)

pinsert :: Ord a => a -> PTree a -> PTree a
pinsert x PLeaf = PNode x PLeaf PLeaf
pinsert x (PNode val ltree rtree)
  | x > val = PNode val ltree (pinsert x rtree)
  | x < val = PNode val (pinsert x ltree) rtree
  | otherwise = PNode val ltree rtree