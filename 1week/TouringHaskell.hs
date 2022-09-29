type Pos = (Int, Int)
data Direction = North | South | East | West

move:: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move West (x,y) = (x-1, y)
move East (x,y) = (x+1, y)

moves :: [Direction] -> Pos -> Pos
moves (x:xs) pos = if length xs > 0 then moves xs (move x pos)
                                    else move x pos

data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord)

nat2int :: Nat -> Int
nat2int (Succ x) = if x == Zero then 1
                                else (nat2int x) + 1
nat2int Zero = 0

int2nat :: Int -> Nat
int2nat i = if i == 0 then Zero
                      else Succ (int2nat (i-1))

data Tree = Leaf | Node Int Tree Tree
    deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node y t1 t2)
    | x > y = Node y t1 (insert x t2)
    | x < y = Node y (insert x t1) t2
    | otherwise = Node y t1 t2

data PTree a = PLeaf | PNode a (PTree a) (PTree a)
    deriving (Eq, Show, Read, Ord)

pinsert :: Ord a => a -> PTree a -> PTree a
pinsert val PLeaf = PNode val PLeaf PLeaf
pinsert val (PNode nval t1 t2) =
    | val > nval = PNode val t1 (pinsert val t2)
    | val < nval = PNode val (pinsert val t1) t2
    | otherwise = PNode nval t1 t2