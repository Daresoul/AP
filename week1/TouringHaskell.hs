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
