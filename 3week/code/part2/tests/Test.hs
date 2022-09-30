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
   testCase "Minus Plus left asso 2-2+2" $ parseString "2-2+2" @?= Right [SExp (Oper Plus (Oper Minus (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 2)))],  
   testCase "Plus Minus left asso 2+2-2" $ parseString "2+2-2" @?= Right [SExp (Oper Minus (Oper Plus (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 2)))],
   testCase "char string test 'lolqopbob' " $ parseString "'lolqopbob'" @?= Right [SExp (Const (StringVal "lolqopbob"))],
   testCase "int string test 'lolqopbob' " $ parseString "'123561636'" @?= Right [SExp (Const (StringVal "123561636"))],
   testCase "symbols string test ')%&!)(#' " $ parseString "')%&!)(#'" @?= Right [SExp (Const (StringVal ")%&!)(#"))],
   testCase "non Ascii Symbols test '£¡½¡¥[±}{]]¥}¡½]}¡[½}'" $ case parseString "'£¡½¡¥[±}{]]¥}¡½]}¡[½}'" of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,  
   testCase "mix string test 'Niowe89359n9t!&&&#&&/()' " $ parseString "'Niowe89359n9t!&&&#&&/()'" @?= Right [SExp (Const (StringVal "Niowe89359n9t!&&&#&&/()"))],
   testCase "strings with keywords test 'ifforinrangelist' " $ parseString "'ifforinrangelist'" @?= Right [SExp (Const (StringVal "ifforinrangelist"))],
   testCase "Dirty string symbol fail" $ case parseString "wow!" of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty addition symbol fail" $ case parseString "2!+2!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty addition letter fail" $ case parseString "2awr+2awr!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty addition symbol2 fail" $ case parseString "2...+2..0!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty minus symbol fail" $ case parseString "2!-2!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty minus letter fail" $ case parseString "2awr-2awr!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty minus symbol2 fail" $ case parseString "2...-2..0!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty times symbol fail" $ case parseString "2!*2!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty times letter fail" $ case parseString "2awr*2awr!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty tiems symbol2 fail" $ case parseString "2...*2..0!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "Times 2*2" $ parseString "2*2" @?= Right [SExp (Oper Times (Const (IntVal 2)) (Const (IntVal 2)))],
   testCase "Times (-2)*2" $ parseString "(-2)*2" @?= Right [SExp (Oper Times (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Times -2*2" $ parseString "-2*2" @?= Right [SExp (Oper Times (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Times (-2)*(-2)" $ parseString "(-2)*(-2)" @?= Right [SExp (Oper Times (Const (IntVal (-2) )) (Const (IntVal (-2))))],
   testCase "Times left asso 2*2*2" $ parseString "2*2*2" @?= Right [SExp (Oper Times (Oper Times (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 2)))]
   ]