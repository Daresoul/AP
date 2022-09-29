-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, lister, stringCat, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\e -> (Right a, []))
  m >>= f = Comp (\e -> case (runComp m e) of
    (Right a, val1) -> case (runComp (f a) e) of
      (Right b, val2) -> (Right b, concat [val1, val2])
      (Left b, val2) -> (Left b, val2)
    (Left a, val1) -> (Left a, val1))

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort re = Comp (\e -> (Left re, []))
  
look :: VName -> Comp Value
look v = Comp (\e -> case lookup v e of
  Just y -> (Right y, [])
  Nothing -> (Left (EBadVar v), []))

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding v val c = Comp (\e -> runComp c ((v, val):e))

output :: String -> Comp ()
output s = Comp (\e -> (Right (), [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy val = case val of
  NoneVal -> False
  TrueVal -> True
  FalseVal -> False
  IntVal x -> if x == 0 then False else True
  StringVal x -> if (length x) > 0 then True else False
  ListVal [] -> False
  ListVal x -> True

operate :: Op -> Value -> Value -> Either String Value
operate Plus v1 v2 = 
  case v1 of
    IntVal x1 -> case v2 of
      IntVal x2 -> Right (IntVal (x1 + x2))
      _ -> Left "2. argument for the plus operator needs to be either IntVal"
    StringVal x1 -> case v2 of
      IntVal x2 -> Right (StringVal (x1 ++ (show x2)))
      _ -> Left "2. argument for the plus operator needs to be either IntVal"
    _ -> Left "1. argument for the plus operator needs to be either IntVal"

operate Minus v1 v2 =
  case v1 of
    IntVal x1 -> case v2 of
      IntVal x2 -> Right (IntVal (x1 - x2))
      _ -> Left "2. argument for the minus operator needs to be IntVal"
    _ -> Left "1. argument for the minus operator needs to be IntVal"

operate Times v1 v2 =
  case v1 of
    IntVal x1 -> case v2 of
      IntVal x2 -> Right (IntVal (x1 * x2))
      _ -> Left "2. argument for the times operator needs to be IntVal"
    _ -> Left "1. argument for the times operator needs to be IntVal"

operate Div v1 v2 =
  case v1 of
    IntVal x1 -> case v2 of
      IntVal x2 -> Right (IntVal (x1 `div` x2))
      _ -> Left "2. argument for the div operator needs to be IntVal"
    _ -> Left "1. argument for the div operator needs to be IntVal"

operate Mod v1 v2 =
  case v1 of
    IntVal x1 -> case v2 of
      IntVal x2 -> Right (IntVal (x1 `mod` x2))
      _ -> Left "2. argument for the mod operator needs to be IntVal"
    _ -> Left "1. argument for the mod operator needs to be IntVal"

operate Eq v1 v2 =
  case v1 of
    IntVal x1 -> case v2 of
      IntVal x2 -> if x1 == x2 then Right TrueVal else Right FalseVal
      FalseVal -> if x1 == 0 then Right TrueVal else Right FalseVal
      TrueVal -> if x1 == 0 then Right FalseVal else Right TrueVal
      _ -> Left "2. argument for the eq operator needs to be either IntVal, FalseVal, TrueVal or StringVal"
    TrueVal -> case v2 of
      IntVal x2 -> if x2 == 0 then Right FalseVal else Right TrueVal
      FalseVal -> Right FalseVal
      TrueVal -> Right TrueVal
      _ -> Left "2. argument for the eq operator needs to be either IntVal, FalseVal, TrueVal or StringVal"
    FalseVal -> case v2 of
      IntVal x2 -> if x2 == 0 then Right TrueVal else Right FalseVal
      FalseVal -> Right TrueVal
      TrueVal -> Right FalseVal
      _ -> Left "2. argument for the eq operator needs to be either IntVal, FalseVal, TrueVal or StringVal"
    StringVal x1 -> case v2 of
      StringVal x2 -> if x1 == x2 then Right TrueVal else Right FalseVal
      _ -> Left "2. argument for the eq operator needs to be either IntVal, FalseVal, TrueVal or StringVal"
    _ -> Left "1. argument for the eq operator needs to be either IntVal, FalseVal, TrueVal or StringVal"

operate Less v1 v2 =
  case v1 of
    IntVal x1 -> case v2 of
      IntVal x2 -> if x1 < x2 then Right TrueVal else Right FalseVal
      StringVal x2 -> if x1 < (length x2) then Right TrueVal else Right FalseVal
      _ -> Left "2. argument for the less operator needs to be either IntVal or StringVal"
    StringVal x1 -> case v2 of
      StringVal x2 -> if ((length x1) < (length x2)) then Right TrueVal else Right FalseVal
      IntVal x2 -> if (length x1) < x2 then Right TrueVal else Right FalseVal
      _ -> Left "2. argument for the less operator needs to be either IntVal or StringVal"
    _ -> Left "1. argument for the less operator needs to be either IntVal or StringVal"

operate Greater v1 v2 =
  case v1 of
    IntVal x1 -> case v2 of
      IntVal x2 -> if x1 > x2 then Right TrueVal else Right FalseVal
      StringVal x2 -> if x1 > (length x2) then Right TrueVal else Right FalseVal
      _ -> Left "2. argument for the greater operator needs to be either IntVal or StringVal"
    StringVal x1 -> case v2 of
      StringVal x2 -> if ((length x1) > (length x2)) then Right TrueVal else Right FalseVal
      IntVal x2 -> if (length x1) > x2 then Right TrueVal else Right FalseVal
      _ -> Left "2. argument for the greater operator needs to be either IntVal or StringVal"
    _ -> Left "1. argument for the greater operator needs to be either IntVal or StringVal"

-- lister :: start val -> max val -> step size -> [IntVal]
lister :: Int -> Int -> Int -> [Value]
lister i n t =
  if t > 0 then
    if i < n then
      IntVal i:lister (i+t) n t
    else
      []
  else
    if i > n then
      IntVal i:lister (i+t) n t
    else
      []

-- stringCat :: list of values -> bool defining if it is inside a list -> resulting string for output.
stringCat :: [Value] -> Bool -> String
stringCat val z = case val of
  x:xs -> case x of
    NoneVal ->
      if (length xs) > 0 then
        if z then "None, " ++ stringCat xs True else "None " ++ stringCat xs False
      else
        "None"

    TrueVal -> 
      if (length xs) > 0 then
        if z then "True, " ++ stringCat xs True else "True " ++ stringCat xs False
      else
        "True"

    FalseVal -> 
      if (length xs) > 0 then
        if z then
          "False, " ++ stringCat xs True
        else
          "False " ++ stringCat xs False
      else
        "False"

    IntVal y -> 
      if (length xs) > 0 then
        if z then
          (show y) ++ ", " ++ stringCat xs True
        else
          (show y) ++ " " ++ stringCat xs False
      else
        (show y)
  
    StringVal y ->
        if (length xs) > 0 then
          if z then y ++ ", " ++ stringCat xs True else y ++ " " ++ stringCat xs False
        else
          y
    
    ListVal y ->
      if (length xs) > 0 then
        if z then
          "[" ++ (stringCat y True) ++ "], " ++ (stringCat xs False)
        else
          "[" ++ (stringCat y True) ++ "] " ++ (stringCat xs False)
      else
        "[" ++ (stringCat y True) ++ "]"
  [] -> ""

apply :: FName -> [Value] -> Comp Value
apply f arg = case f of
  "range" ->
    case arg of
      [IntVal arg1, IntVal arg2, IntVal arg3] ->
        if arg3 == 0 then
          Comp (\e -> (Left (EBadArg "Step size cant be 0"), []))
        else
          Comp (\e -> (Right (ListVal (lister arg1 arg2 arg3)), []))
      [IntVal arg1, IntVal arg2] -> Comp (\e -> (Right (ListVal (lister arg1 arg2 1)), []))
      [IntVal arg1] -> Comp (\e -> (Right (ListVal (lister 0 arg1 1)), []))
      _ -> Comp (\e -> (Left (EBadArg "Values could not be parsed to IntVal"), []))
  "print" -> Comp (\e -> (Right (NoneVal), [(stringCat arg False)]))
  _ -> Comp (\e -> (Left (EBadFun f), []))


-- Main functions of interpreter
eval :: Exp -> Comp Value
eval exp = Comp (\e -> case exp of
    Const x -> (Right x, [])
    Var s -> ()
    --Var x -> case look x of
    --  Comp v -> (Right v, [])
  )

exec :: Program -> Comp ()
exec p = undefined--Comp (\e -> case p of
  --x:xs -> case x of
  --  SDef v exp -> case (eval exp) of
  --    Comp val -> val
  --)

execute :: Program -> ([String], Maybe RunError)
execute = undefined
