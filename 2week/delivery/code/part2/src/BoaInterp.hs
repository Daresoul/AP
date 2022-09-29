-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\_ -> (Right a, mempty))
  m >>= f = Comp (\e -> case runComp m e of
    (Right a, xs) -> case runComp (f a) e of
      (Right b, ys) -> (Right b, (++) xs ys)
      (Left b, ys) -> (Left b, (++) xs ys)
    (Left a, xs) -> (Left a, xs)

    )

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort re = Comp (\_ -> (Left re, []))
  
look :: VName -> Comp Value
look v = Comp (\e -> case lookup v e of
  Just y -> (Right y, [])
  Nothing -> (Left (EBadVar v), []))

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding v val c = Comp (\e -> runComp c ((v, val):e))

output :: String -> Comp ()
output s = Comp (\_ -> (Right (), [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy val = case val of
  NoneVal -> False
  TrueVal -> True
  FalseVal -> False
  IntVal x -> if x == 0 then False else True -- should be done with pattern matching instead of if
  StringVal x -> if (length x) > 0 then True else False -- should be done with pattern matching instead of if
  ListVal [] -> False
  ListVal _ -> True

operate :: Op -> Value -> Value -> Either String Value
operate Plus v1 v2 =
  case v1 of
    IntVal x1 -> case v2 of -- Could have been done with pattern matching instead of case of since we are only looking for a single value
      IntVal x2 -> Right (IntVal (x1 + x2))
      _ -> Left "2. argument for the plus operator needs to be IntVal"
    _ -> Left "1. argument for the plus operator needs to be IntVal"

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
      IntVal x2 -> if x2 /= 0 then Right (IntVal (x1 `div` x2))
                              else Left "Cannot divide by Zero"
      _ -> Left "2. argument for the div operator needs to be IntVal"
    _ -> Left "1. argument for the div operator needs to be IntVal"
  
operate Mod v1 v2 =
  case v1 of
    IntVal x1 -> case v2 of
      IntVal x2 -> if x2 /= 0 then Right (IntVal (x1 `mod` x2))
                              else Left "Cannot divide by Zero"
      _ -> Left "2. argument for the mod operator needs to be IntVal"
    _ -> Left "1. argument for the mod operator needs to be IntVal"
operate In _ (ListVal []) = Right FalseVal
operate In v1 (ListVal (x:xs)) = case operate Eq v1 x of
  Right TrueVal -> Right TrueVal
  Right FalseVal -> operate In v1 (ListVal xs)
  Right _ -> Left "Will never happen."
  Left y -> Left y
operate In _ _ = Left "Arguments need to be a value, and a ListVal"

operate Eq v1 v2 = if v1 == v2 then Right TrueVal else Right FalseVal

operate Less (IntVal v1) (IntVal v2) = if v1 < v2 then Right TrueVal else Right FalseVal
operate Less _ _ = Left "Both values has to be int values."

operate Greater (IntVal v1) (IntVal v2) = if v1 > v2 then Right TrueVal else Right FalseVal
operate Greater _ _ = Left "Both values has to be int values."

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

concatenatePrintString :: [Value] -> Bool -> String -- Should rewrite very hard to understand, maybe some sub functions
concatenatePrintString val z = case val of
  x:xs -> case x of
    NoneVal ->
      if length xs > 0 then
        if z then "None, " ++ concatenatePrintString xs True else "None, " ++ concatenatePrintString xs False 
      else
        "None"

    IntVal y -> 
      if length xs > 0 then
        if z then
          show y ++ ", " ++ concatenatePrintString xs True
        else
          show y ++ " " ++ concatenatePrintString xs False
      else
        show y

    ListVal y ->
      if length xs > 0 then
        "[" ++ concatenatePrintString y True ++ "], " ++ concatenatePrintString xs False
      else
        "[" ++ concatenatePrintString y True ++ "]"

    TrueVal -> 
      if length xs > 0 then
        if z then "True, " ++ concatenatePrintString xs True else "True " ++ concatenatePrintString xs False
      else
        "True"

    FalseVal -> 
      if length xs > 0 then
        if z then
          "False, " ++ concatenatePrintString xs True
        else
          "False " ++ concatenatePrintString xs False
      else
        "False"
  
    StringVal y ->
        if length xs > 0 then
          if z then y ++ ", " ++ concatenatePrintString xs True else y ++ " " ++ concatenatePrintString xs False
        else
          y
  [] -> ""

apply :: FName -> [Value] -> Comp Value
apply f arg = case f of
  "range" ->
    case arg of
      [IntVal arg1, IntVal arg2, IntVal arg3] ->
        if arg3 == 0 then
          abort (EBadArg "Step size cant be 0")
        else
          return (ListVal (lister arg1 arg2 arg3)) -- returning the values from the auxilery function
      [IntVal arg1, IntVal arg2] -> return (ListVal (lister arg1 arg2 1))
      [IntVal arg1] -> return (ListVal (lister 0 arg1 1))
      _ -> abort (EBadArg "Values could not be parsed to IntVal")

  "print" -> Comp (\_ -> (Right (NoneVal), [(concatenatePrintString arg False)]))
  _ -> abort (EBadFun f)

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval e = case e of
  Const v -> return v 

  Var s -> look s
  
  Oper op exp1 exp2 -> do x1 <- eval exp1
                          x2 <- eval exp2
                          case operate op x1 x2 of
                            Right x -> return x
                            Left s -> abort (EBadArg s)

  Not exp -> do x1 <- eval exp
                if truthy x1 == True -- Just gonna return not (truthy x1) as a Value
                  then return FalseVal
                  else return TrueVal

  Call f xs -> do x1 <- eval (List xs)
                  case x1 of
                    ListVal ys -> apply f ys
                    _ -> abort (EBadArg "List didnt evaluate to a list")

  List xs -> let x1 = fmap (\x -> eval x) xs in do x1 <- sequence x1 -- sequence just makes our [Comp Value] into Comp Value
                                                   return (ListVal x1)

                
  Compr _ [] -> return (ListVal [])
  Compr exp (x:xs) -> case x of
    CCFor vname exp2 -> do x2 <- eval exp2
                           case x2 of
                            ListVal [] -> return (ListVal [])
                            ListVal ys -> let x1 = fmap (\y -> withBinding vname y (eval exp)) ys
                              in do x4 <- eval (Compr exp xs)
                                    x3 <- sequence x1
                                    case x4 of
                                      ListVal zs -> case x3 of
                                        is -> return (ListVal ((++) is zs))
                                      _ -> abort (EBadArg "The given expressions isnt a ListVal")
                            _ -> abort (EBadArg "The given expressions isnt a ListVal")

    CCIf exp2 -> do x2 <- eval exp2
                    if truthy x2 == True
                      then do x1 <- eval (Compr exp xs)
                              case x1 of
                                ListVal ys -> return (ListVal ((++) [x2] ys))
                                _ -> abort (EBadArg "Is not a list2")
                      else return (ListVal [])

exec :: Program -> Comp ()
exec p = let x1 = fmap (\s -> case s of
                          SDef vname exp -> do y1 <- eval exp
                                               withBinding vname y1 (return ()) -- Is not saved to a global env for the next computations
                          SExp exp -> do _ <- eval exp -- just to evaluate it
                                         return ()) p
            in do _ <- sequence x1 -- Just to evaluate it will not be in the a since it return Comp ()
                  return ()

execute :: Program -> ([String], Maybe RunError)
execute = undefined