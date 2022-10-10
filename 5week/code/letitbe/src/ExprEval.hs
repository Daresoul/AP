module ExprEval where

import ExprAst
import qualified Data.Map.Strict as M
import Data.Map(Map)

type Env = Map String Int

oper :: Op -> (Int -> Int -> Int)
oper Plus = (+)
oper Minus = (-)
oper Times = (*)

eval :: Expr -> Env -> Either String Int
eval (Const n) env = return n
eval (Oper op x y) env = (oper op) <$> eval x env <*> eval y env
eval (Var v) env = case M.lookup v env of
                     Nothing -> Left ("Unknown identifier: "++v)
                     Just val -> return val
eval (Let v e body) env = do
  val <- eval e env
  eval body $ M.insert v val env
evalTop e = eval e M.empty

simplify :: Expr -> Expr
simplify e =
  case e of
    Oper Plus (Const 0) a -> a
    Oper Plus a (Const 0) -> a
    Oper Plus (Const c1) (Const c2) -> Const(c1+c2)
    Oper Minus a (Const 0) -> a
    Oper Minus (Const 0) (Const a) -> Const(-a)
    Oper Minus (Const c1) (Const c2) -> Const(c1-c2)
    Oper Times (Const 0) _ -> Const 0
    Oper Times _ (Const 0) -> Const 0
    Oper Times (Const 1) a -> a
    Oper Times a (Const 1) -> a
    Oper Times (Const a) (Const (-1)) -> Const(-a)
    Oper Times (Const (-1)) (Const a) -> Const(-a)
    Oper Times (Const c1) (Const c2) -> Const(c1*c2)
    Oper op e1 e2 -> Oper op (simplify e1) (simplify e2)
    Let v e body -> if searchForVarInTree body v  then Let v (simplify e) (simplify body)
                                                  else simplify body
    _ -> e

searchForVarInTree :: Expr -> String -> Bool
searchForVarInTree e s =
  case e of
    Const _ -> False
    Oper _ e1 e2 -> (searchForVarInTree e1 s) || (searchForVarInTree e2 s)
    Var s2 -> if s == s2  then True
                          else False
    Let v e body -> if v == s then if searchForVarInTree e s then True
                                                             else False
                              else searchForVarInTree e s || searchForVarInTree body s
