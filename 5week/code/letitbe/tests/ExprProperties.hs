module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
import qualified Data.Map.Strict as M

exprN :: Int -> Gen Expr
exprN 0 = oneof [
          fmap Const arbitrary
          , do  name <- arbitrary :: Gen Char
                return $ Var [name]
          ]
exprN n = oneof
         [ exprN 0
         , do x <- exprN  (n `div` 2)
              y <- exprN  (n `div` 2)
              return $ Oper Plus x y
         , do x <- exprN (n `div` 2)
              y <- exprN (n `div` 2)
              return $ Oper Minus x y
         , do x <- exprN (n `div` 2)
              y <- exprN (n `div` 2)
              return $ Oper Times x y
         , do exp <- exprN (n `div` 2)
              body <- exprN (n `div` 2)
              name <- arbitrary :: Gen Char
              return $ Let [name] exp body
         ]

instance Arbitrary Expr where
   arbitrary = sized exprN

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = E.eval (E.simplify x) M.empty  === E.eval x M.empty