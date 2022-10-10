module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
import qualified Data.Map.Strict as M

exprN :: Int -> Gen Expr
exprN 0 = fmap Const arbitrary
exprN n = oneof
         [ fmap Const arbitrary
         , do x <- exprN  (n `div` 2)
              y <- exprN  (n `div` 2)
              return $ Oper Plus x y
         ]

instance Arbitrary Expr where
   arbitrary = sized exprN

prop_eval_simplify :: Expr -> Property

prop_eval_simplify x = E.eval (E.simplify x) M.empty  === E.eval x M.empty