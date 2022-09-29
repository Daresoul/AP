-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Stubby tests"
  [ testCase "Const" $
    runComp (eval (Const (IntVal 5))) []
      @?= (Right (IntVal 5), []),
    
    testCase "var" $
    runComp (eval (Var "x")) [("x", IntVal 5)]
      @?= (Right (IntVal 5), []),
    
    testCase "oper" $
    runComp (eval (Oper Div (Const (IntVal 0)) (Var "x"))) [("x", IntVal 5)]
      @?= (Right (IntVal 0), []),

    testCase "not" $
    runComp (eval (Not (Const (TrueVal)))) []
      @?= (Right (FalseVal), []),
    
    testCase "call" $
    runComp (eval (Call "range" [Const (IntVal 3), Const (IntVal 6)])) []
      @?= (Right (ListVal [IntVal 3, IntVal 4, IntVal 5]), []),
    
    testCase "list" $
    runComp (eval (List [Const (IntVal 3), Const (IntVal 6), Var "x"])) [("x", StringVal "123")]
      @?= (Right (ListVal [IntVal 3, IntVal 6, StringVal "123"]), []),
    
    testCase "Compr CCIF" $
    runComp (eval (Compr (Var "x") [CCIf (Var "y")])) [("y", TrueVal)]
      @?= (Right (ListVal [TrueVal]), []),

    testCase "Compr CCFOR" $
    runComp (eval (Compr (Oper Times (Var "x") (Var "x")) [CCFor "x" (Const (ListVal [IntVal 3]))])) []
      @?= (Right (ListVal [IntVal 9]), []),

    testCase "Compr Mult" $
    runComp (eval (Compr (Oper Times (Var "x") (Var "x")) [CCIf (Const (StringVal "123")), CCFor "x" (Const (ListVal [IntVal 3])), CCFor "x" (Const (ListVal [IntVal 4])), CCIf (Const (IntVal 3))])) []
      @?= (Right (ListVal [StringVal "123", IntVal 9, IntVal 16, IntVal 3]), []),

    testCase "Compr Mult2" $
    runComp (eval (Compr (Oper Times (Var "x") (Var "y")) [CCFor "x" (Const (ListVal [IntVal 3, IntVal 5])), CCFor "y" (Const (ListVal [IntVal 1, IntVal 2]))])) []
      @?= (Right (ListVal [StringVal "123", IntVal 9, IntVal 16, IntVal 3]), []),
    
    testCase "exec var with binding" $
    runComp (exec [SDef "a" (Const (IntVal 3)), SExp (Var "a")]) []
      @?= (Right (),[]),

    testCase "exec var with binding in env" $
    runComp (exec [SExp (Var "a")]) [("a", IntVal 3)]
      @?= (Right (),[]),

    testCase "exec Const" $
    runComp (exec [SExp (Const (IntVal 3))]) []
      @?= (Right (),[]),
    
    testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),

   testCase "execute misc.ast from handout" $
     do pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing)
  ]