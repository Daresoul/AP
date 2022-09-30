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
   testCase "Dirty variable symbol fail" $ case parseString "wow!" of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty addition symbol fail" $ case parseString "2!+2!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty addition letter fail" $ case parseString "2awr+2awr!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty addition symbol2 fail" $ case parseString "2...+2..0!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty minus symbol fail" $ case parseString "2!-2!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty minus letter fail" $ case parseString "2awr-2awr!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty minus symbol2 fail" $ case parseString "2...-2..0!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty times symbol fail" $ case parseString "2!*2!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty times letter fail" $ case parseString "2awr*2awr!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty tims symbol2 fail" $ case parseString "2...*2..0!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty division symbol fail" $ case parseString "2!//2!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty division letter fail" $ case parseString "2awr//2awr!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty division symbol2 fail" $ case parseString "2...//2..0!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty Modulus symbol fail" $ case parseString "2!%2!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty Modulus letter fail" $ case parseString "2awr%2awr!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "dirty Modulus symbol2 fail" $ case parseString "2...%2..0!"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "Times 2*2" $ parseString "2*2" @?= Right [SExp (Oper Times (Const (IntVal 2)) (Const (IntVal 2)))],
   testCase "Times (-2)*2" $ parseString "(-2)*2" @?= Right [SExp (Oper Times (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Times -2*2" $ parseString "-2*2" @?= Right [SExp (Oper Times (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Times (-2)*(-2)" $ parseString "(-2)*(-2)" @?= Right [SExp (Oper Times (Const (IntVal (-2) )) (Const (IntVal (-2))))],
   testCase "Times left asso 2*2*2" $ parseString "2*2*2" @?= Right [SExp (Oper Times (Oper Times (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 2)))],
   testCase "Division 2*2" $ parseString "2//2" @?= Right [SExp (Oper Div (Const (IntVal 2)) (Const (IntVal 2)))],
   testCase "Division (-2)/2" $ parseString "(-2)//2" @?= Right [SExp (Oper Div (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Division -2/2" $ parseString "-2//2" @?= Right [SExp (Oper Div (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Division (-2)/(-2)" $ parseString "(-2)//(-2)" @?= Right [SExp (Oper Div (Const (IntVal (-2) )) (Const (IntVal (-2))))],
   testCase "Division left asso 2/2/2" $ parseString "2//2//2" @?= Right [SExp (Oper Div (Oper Div (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 2)))],
   testCase "Modulus 2%2" $ parseString "2%2" @?= Right [SExp (Oper Mod (Const (IntVal 2)) (Const (IntVal 2)))],
   testCase "Modulus (-2)%2" $ parseString "(-2)%2" @?= Right [SExp (Oper Mod (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Modulus -2%2" $ parseString "-2%2" @?= Right [SExp (Oper Mod (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Modulus (-2)%(-2)" $ parseString "(-2)%(-2)" @?= Right [SExp (Oper Mod (Const (IntVal (-2) )) (Const (IntVal (-2))))],
   testCase "Modulus left asso 2%2%2" $ parseString "2%2%2" @?= Right [SExp (Oper Mod (Oper Mod (Const (IntVal 2)) (Const (IntVal 2))) (Const (IntVal 2)))],
   testCase "Equals 2==2" $ parseString "2==2" @?= Right [SExp (Oper Eq (Const (IntVal 2)) (Const (IntVal 2)))],
   testCase "Equals (-2)==2" $ parseString "(-2)==2" @?= Right [SExp (Oper Eq (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Equals -2==2" $ parseString "-2==2" @?= Right [SExp (Oper Eq (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Equals (-2)==(-2)" $ parseString "(-2)==(-2)" @?= Right [SExp (Oper Eq (Const (IntVal (-2) )) (Const (IntVal (-2))))],
   testCase "Less 2<2" $ parseString "2<2" @?= Right [SExp (Oper Less (Const (IntVal 2)) (Const (IntVal 2)))],
   testCase "Less (-2)<2" $ parseString "(-2)<2" @?= Right [SExp (Oper Less (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Less -2<2" $ parseString "-2<2" @?= Right [SExp (Oper Less (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Less (-2)<(-2)" $ parseString "(-2)<(-2)" @?= Right [SExp (Oper Less (Const (IntVal (-2) )) (Const (IntVal (-2))))],
   testCase "Greater 2>2" $ parseString "2>2" @?= Right [SExp (Oper Greater (Const (IntVal 2)) (Const (IntVal 2)))],
   testCase "Greater (-2)>2" $ parseString "(-2)>2" @?= Right [SExp (Oper Greater (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Greater -2>2" $ parseString "-2>2" @?= Right [SExp (Oper Greater (Const (IntVal (-2) )) (Const (IntVal 2)))],
   testCase "Greater (-2)>(-2)" $ parseString "(-2)>(-2)" @?= Right [SExp (Oper Greater (Const (IntVal (-2) )) (Const (IntVal (-2))))],
   testCase "LessOrEqual 1<=2" $ parseString "1<=2" @?= Right [SExp (Oper Less (Const (IntVal 1)) (Oper Plus (Const (IntVal 2)) (Const (IntVal 1))))],
   testCase "LessOrEqual -1<=-2" $ parseString "-1<=-2" @?= Right [SExp (Oper Less (Const (IntVal (-1))) (Oper Plus (Const (IntVal (-2))) (Const (IntVal 1))))],
   testCase "LessOrEqual 1<=-2" $ parseString "1<=-2" @?= Right [SExp (Oper Less (Const (IntVal 1)) (Oper Plus (Const (IntVal (-2))) (Const (IntVal 1))))],
   testCase "LessOrEqual -1<=2" $ parseString "-1<=2" @?= Right [SExp (Oper Less (Const (IntVal (-1))) (Oper Plus (Const (IntVal 2)) (Const (IntVal 1))) )],
   testCase "GreaterOrEqual 1>=2" $ parseString "1>=2" @?= Right [SExp (Oper Less (Const (IntVal 2)) (Oper Plus (Const (IntVal 1)) (Const (IntVal 1))))],
   testCase "GreaterOrEqual -1>=-2" $ parseString "-1>=-2" @?= Right [SExp (Oper Less (Const (IntVal (-2))) (Oper Plus (Const (IntVal (-1))) (Const (IntVal 1))))],
   testCase "GreaterOrEqual 1>=-2" $ parseString "1>=-2" @?= Right [SExp (Oper Less (Const (IntVal (-2))) (Oper Plus (Const (IntVal (1))) (Const (IntVal 1))))],
   testCase "GreaterOrEqual -1>=2" $ parseString "-1>=2" @?= Right [SExp (Oper Less (Const (IntVal 2)) (Oper Plus (Const (IntVal (-1))) (Const (IntVal 1))))],
   
   testCase "Precedens Simple 1 (Minus)" $ parseString "1-2-3" @?= Right [SExp (Oper Minus (Oper Minus (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
   testCase "Precedens Simple 2 (Plus)" $ parseString "1+2+3" @?= Right [SExp (Oper Plus (Oper Plus (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
   testCase "Precedens Simple 2 (Times)" $ parseString "1*2*3" @?= Right [SExp (Oper Times (Oper Times (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
   testCase "Precedens Simple 2 (Div)" $ parseString "1//2//3" @?= Right [SExp (Oper Div (Oper Div (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],   

   testCase "Precedens Medium 1 (Minus times)" $ parseString "1-2*3" @?= Right [SExp (Oper Minus (Const (IntVal 1)) (Oper Times (Const (IntVal 2)) (Const (IntVal 3))))],
   testCase "Precedens Medium 2 (Plus times)" $ parseString "1+2*3" @?= Right [SExp (Oper Plus (Const (IntVal 1)) (Oper Times (Const (IntVal 2)) (Const (IntVal 3))))],
   testCase "Precedens Medium 2 (Times Div)" $ parseString "1*2//3" @?= Right [SExp (Oper Div (Oper Times (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
   testCase "Precedens Medium 2 (Div Times)" $ parseString "1//2*3" @?= Right [SExp (Oper Times (Oper Div (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],   
   
   testCase "Precedens Hard 1 (Paras Minus times)" $ parseString "(1-2)*3" @?= Right [SExp (Oper Times (Oper Minus (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
   testCase "Precedens Hard 2 (Paras Plus times)" $ parseString "(1+2)*3" @?= Right [SExp (Oper Times (Oper Plus (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],
   testCase "Precedens Hard 2 (Paras Times Div)" $ parseString "1*(2//3)" @?= Right [SExp (Oper Times (Const (IntVal 1)) (Oper Div (Const (IntVal 2)) (Const (IntVal 3))))],
   testCase "Precedens Hard 2 (Paras Div Times)" $ parseString "1//(2*3)" @?= Right [SExp (Oper Div (Oper Times (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))],   

   testCase "Equals 3xfail 2==2==2 fail" $ case parseString "2==2==2"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "Greater Equal 3xfail 2>=2>=2 fail" $ case parseString "2>=2>=2"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "Less 3xfail 2<2<2 fail" $ case parseString "2<2<2"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "Greater 3xfail 2>2>2 fail" $ case parseString "2>2>2"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,
   testCase "Less Equal 3xfail 2<=2<=2 fail" $ case parseString "2<=2<=2"  of Left e -> return (); Right p -> assertFailure $ "Unexpected parse: " ++ show p,

   testCase "List empty" $ parseString " [  ] " @?= Right [SExp (List [])],
   testCase "List values" $ parseString " ['hej', 'med', 'dig', 1] " @?= Right [SExp (List [Const (StringVal "hej"), Const (StringVal "med"), Const (StringVal "dig"), Const (IntVal 1)])],
   
   testCase "List Comprehension" $ parseString "[x for y in 2+2 if 3]" @?= Right [SExp (Compr (Var "x") [CCFor "y" (Oper Plus (Const (IntVal 2)) (Const (IntVal 2))),CCIf (Const (IntVal 3))])],
   testCase "List Comprehension advanced" $ parseString "[x for y in 2+2 if 3 if 4 for z in 5*5]" @?= Right [SExp (Compr (Var "x") [CCFor "y" (Oper Plus (Const (IntVal 2)) (Const (IntVal 2))),CCIf (Const (IntVal 3)),CCIf (Const (IntVal 4)),CCFor "z" (Oper Times (Const (IntVal 5)) (Const (IntVal 5)))])],
   ]