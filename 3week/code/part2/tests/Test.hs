-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "AllTests" [testingMathOperators]

testingMathOperators :: TestTree
testingMathOperators = testGroup "testingMathOperators"
  [testCase "Plus 2+2" $ parseString "2+2" @?= Right [SExp (Oper Plus (Const (IntVal 2)) (Const (IntVal 2)))],
   testCase "Plus (-2)+2" $ parseString "(-2)+2" @?= Right [SExp (Oper Plus (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Plus -2+2" $ parseString "-2+2" @?= Right [SExp (Oper Plus (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Plus (-2)(-2)" $ parseString "(-2)+(-2)" @?= Right [SExp (Oper Plus (Const (IntVal (-2) )) (Const (IntVal (-2))))],
   testCase "Plus left asso 2+2+2" $ parseString "2+2+2" @?= Right [SExp (Oper Plus (Oper Plus (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 2)))],
   testCase "Minus 2-2" $ parseString "2-2" @?= Right [SExp (Oper Minus (Const (IntVal 2)) (Const (IntVal 2)))],
   testCase "Minus -2-2" $ parseString "-2-2" @?= Right [SExp (Oper Minus (Const (IntVal (-2))) (Const (IntVal 2)))],
   testCase "Minus (-2)(-2)" $ parseString "(-2)-(-2)" @?= Right [SExp (Oper Minus (Const (IntVal (-2))) (Const (IntVal (-2))))],
   testCase "Minus left asso 2-2-2" $ parseString "2-2-2" @?= Right [SExp (Oper Minus (Oper Minus (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 2)))],  
   ]

-- testingFuncEval :: TestTree
-- testingFuncEval = testGroup "TestingFuncEval"
