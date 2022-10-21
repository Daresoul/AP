import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import ExprAst
import qualified ExprEval as E
import qualified ExprProperties as EP

main :: IO ()
main = defaultMain testsuite

testsuite =
  testGroup "Testing expression evaluation and simplification"
  [ testGroup "A few unit-tests"
    [ testCase "Eval: 2+2"
      (Right 4 @=? E.evalTop (Oper Plus (Const 2) (Const 2)))
    , testCase "Eval: let x = 3 in x * x"
      (Right 9 @=? E.evalTop (Let "x" (Const 3)
                              (Oper Times (Var "x") (Var "x"))))
    , testCase "Simplify: x + (2 + 2)"
      (Oper Plus (Var "x") (Const 4) @=?
          E.simplify (Oper Plus (Var "x") (Oper Plus (Const 2) (Const 2))))
    , testCase "Let test for searchForVarInTree variable exp not eval"
          (Const 4 @=?
              E.simplify (Let "x" (Const 5) (Oper Plus (Const 2) (Const 2))))
    , testCase "Let test for searchForVarInTree variable exp eval"
          (Let "x" (Const 5) (Oper Plus (Var "x") (Const 6)) @=?
              E.simplify (Let "x" (Const 5) (Oper Plus (Var "x") (Oper Times (Const 2) (Const 3)))))
    ]
  , quickChecks
  ]

quickChecks =
  testGroup "QuickCheck tests"
  [ testProperty "Evaluating a simplified expression does not change its meaning"
    EP.prop_eval_simplify,
    testProperty "Evaluating a simplified expression does is not longer than the original expression"
    EP.prop_simplify_length
  ]

