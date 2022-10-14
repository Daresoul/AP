module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
import qualified Data.Map.Strict as M

exprN :: Int -> [String] -> Gen Expr
exprN 0 [] = fmap Const arbitrary
exprN 0 list =
  oneof [
    fmap Const arbitrary
    , do  elem <- elements list
          return $ Var (elem)
  ]
exprN n list =
  oneof [
    exprN 0 list,
    do  x <- exprN  (n `div` 2) list
        y <- exprN  (n `div` 2) list
        return $ Oper Plus x y,
    do  x <- exprN (n `div` 2) list
        y <- exprN (n `div` 2) list
        return $ Oper Minus x y,
    do  x <- exprN (n `div` 2) list
        y <- exprN (n `div` 2) list
        return $ Oper Times x y,
    do  name <- arbitrary :: Gen Char
        exp <- exprN (n `div` 2) list
        body <- exprN (n `div` 2) ((++) list [[name]])
        return $ Let [name] exp body
  ]

instance Arbitrary Expr where
   arbitrary = sized (\n -> exprN n [])

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = E.eval (E.simplify x) M.empty === E.eval x M.empty

prop_simplify_length :: Expr -> Property
prop_simplify_length x = (count_expressions (E.simplify x) <= count_expressions x) === True

count_expressions :: Expr -> Int
count_expressions e =
  case e of
    Oper _ exp1 exp2 -> (count_expressions exp1) + (count_expressions exp2)
    Var s -> 1
    Const n -> 1
    Let v exp body -> (count_expressions exp) + (count_expressions body)