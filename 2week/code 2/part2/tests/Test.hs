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
  [
   testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
   testCase "execute misc.ast from handout" $
     do pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing),

   testCase "abort \"x\" bad var" $
    runComp (abort (EBadVar "x")) []
      @?= (Right (IntVal 1),[]), -- will fail but will give and error instead expected value is (Left (EBadVar "x"),[])
   testCase "abort \"x\" bad fun" $
    runComp (abort (EBadFun "x")) []
      @?= (Right (IntVal 1),[]), -- will fail but will give and error instead expected value is (Left (EBadFun "x"),[])
   testCase "abort \"x\" bad arg" $
    runComp (abort (EBadArg "x")) []
      @?= (Right (IntVal 1),[]), -- will fail but will give and error instead expected value is (Left (EBadArg "x"),[])


   testCase "look \"x\"" $
    runComp (look "x") [("x", IntVal 1)]
      @?= (Right (IntVal 1),[]),
   testCase "look \"x\" double value" $
     runComp (look "x") [("x", IntVal 2),("x", IntVal 1)]
      @?= (Right (IntVal 2),[]),
   testCase "look \"x\" does not exist" $
     runComp (look "x") []
      @?= (Left (EBadVar "x"),[]),


   testCase "output \"test 123\"" $
     runComp (output "test 123") []
      @?= (Right (),["test 123"]),


   testCase "Truthy NoneVal" $
    truthy NoneVal
      @?= False,
   testCase "Truthy TrueVal" $
    truthy TrueVal
      @?= True,
   testCase "Truthy FalseVal" $
    truthy FalseVal
      @?= False,
   testCase "Truthy IntVal 0" $
    truthy (IntVal 0)
      @?= False,
   testCase "Truthy IntVal 1" $
    truthy (IntVal 1)
      @?= True,
   testCase "Truthy StringVal empty" $
    truthy (StringVal "")
      @?= False,
   testCase "Truthy StringVal non empty" $
    truthy (StringVal "abc")
      @?= True,
   testCase "Truthy ListVal all true" $
    truthy (ListVal [IntVal 1, StringVal "abc", TrueVal])
      @?= True,
   testCase "Truthy ListVal not all true" $
    truthy (ListVal [IntVal 1, StringVal "abc", FalseVal])
      @?= False,
   testCase "Truthy ListVal inner list all true" $
    truthy (ListVal [IntVal 1, ListVal [StringVal "abc", IntVal 32], TrueVal])
      @?= True,
   testCase "Truthy ListVal inner list not all true" $
    truthy (ListVal [IntVal 1, ListVal [StringVal "abc", IntVal 0], TrueVal])
      @?= False,
    

    testCase "Operate Plus Int Int" $
    operate Plus (IntVal 1) (IntVal 1)
      @?= Right (IntVal 2),
   testCase "Operate Plus Int String" $
    operate Plus (IntVal 1) (StringVal "123")
      @?= Right (StringVal "1123"),
   testCase "Operate Plus String Int" $
    operate Plus (StringVal "123") (IntVal 1)
      @?= Right (StringVal "1231"),
   testCase "Operate Plus String String" $
    operate Plus (StringVal "123") (StringVal "321")
      @?= Right (StringVal "123321"),
   testCase "Operate Plus Int List" $
    operate Plus (IntVal 1) (ListVal [])
      @?= Left "2. argument for the plus operator needs to be either IntVal or StringVal",
   testCase "Operate Plus List Int" $
    operate Plus (ListVal []) (IntVal 1)
      @?= Left "1. argument for the plus operator needs to be either IntVal or StringVal",

   testCase "Operate Minus Int Int" $
    operate Minus (IntVal 3) (IntVal 2)
      @?= Right (IntVal 1),
   testCase "Operate Minus Int List" $
    operate Minus (IntVal 3) (ListVal [])
      @?= Left "2. argument for the minus operator needs to be IntVal",

   testCase "Operate Times Int Int" $
    operate Times (IntVal 3) (IntVal 2)
      @?= Right (IntVal 6),
   testCase "Operate Times Int List" $
    operate Times (IntVal 3) (ListVal [])
      @?= Left "2. argument for the times operator needs to be IntVal",

   testCase "Operate Div Int Int" $
    operate Div (IntVal 4) (IntVal 2)
      @?= Right (IntVal 2),
   testCase "Operate Div Int List" $
    operate Div (IntVal 3) (ListVal [])
      @?= Left "2. argument for the div operator needs to be IntVal",

   testCase "Operate Mod Int Int" $
    operate Mod (IntVal 3) (IntVal 2)
      @?= Right (IntVal 1),
   testCase "Operate Mod Int List" $
    operate Mod (IntVal 3) (ListVal [])
      @?= Left "2. argument for the mod operator needs to be IntVal",

   testCase "Operate Eq Int Int Success" $
    operate Eq (IntVal 3) (IntVal 3)
      @?= Right TrueVal,
   testCase "Operate Eq Int Int Fail" $
    operate Eq (IntVal 3) (IntVal 2)
      @?= Right FalseVal,
   testCase "Operate Eq String String Success" $
    operate Eq (StringVal "123") (StringVal "123")
      @?= Right TrueVal,
   testCase "Operate Eq String String Fail" $
    operate Eq (StringVal "123") (StringVal "321")
      @?= Right FalseVal,
   testCase "Operate Eq True False" $
    operate Eq (TrueVal) (FalseVal)
      @?= Right FalseVal, 
   testCase "Operate Eq False True" $
    operate Eq (FalseVal) (TrueVal)
      @?= Right FalseVal, 
   testCase "Operate Eq True True" $
    operate Eq (TrueVal) (TrueVal)
      @?= Right TrueVal, 
   testCase "Operate Eq False False" $
    operate Eq (FalseVal) (FalseVal)
      @?= Right TrueVal, 
   testCase "Operate Eq Int List" $
    operate Eq (IntVal 3) (ListVal [])
      @?= Left "2. argument for the eq operator needs to be either IntVal, FalseVal, TrueVal or StringVal",
      
   testCase "Operate Less Int Int" $
    operate Less (IntVal 3) (IntVal 4)
      @?= Right TrueVal, 
   testCase "Operate Less Int String" $
    operate Less (IntVal 3) (StringVal "1234")
      @?= Right TrueVal, 
   testCase "Operate Less Int List" $
    operate Less (IntVal 3) (ListVal [])
      @?= Left "2. argument for the less operator needs to be either IntVal or StringVal",

   testCase "Operate Greater Int Int" $
    operate Greater (IntVal 3) (IntVal 4)
      @?= Right FalseVal, 
   testCase "Operate Greater Int String" $
    operate Greater (IntVal 3) (StringVal "1234")
      @?= Right FalseVal, 
   testCase "Operate Greater Int List" $
    operate Greater (IntVal 3) (ListVal [])
      @?= Left "2. argument for the greater operator needs to be either IntVal or StringVal",


   testCase "apply range 1 10 1" $
    runComp (apply "range" [IntVal 1,IntVal 10,IntVal 1]) []
      @?= (Right (ListVal [IntVal 1,IntVal 2,IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9]),[]),
   testCase "apply range 3 10 2" $
    runComp (apply "range" [IntVal 3,IntVal 11,IntVal 2]) [] -- from the assignment
      @?= (Right (ListVal [IntVal 3,IntVal 5, IntVal 7, IntVal 9]),[]),
   testCase "apply range 1 10" $
    runComp (apply "range" [IntVal 3,IntVal 11]) []
      @?= (Right (ListVal [IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9,IntVal 10]),[]),
   testCase "apply range 10" $
    runComp (apply "range" [IntVal 10]) []
      @?= (Right (ListVal [IntVal 0,IntVal 1,IntVal 2, IntVal 3,IntVal 4,IntVal 5,IntVal 6,IntVal 7,IntVal 8,IntVal 9]),[]),
   testCase "apply range 10" $
    runComp (apply "range" [IntVal 1, IntVal 2, IntVal 0]) []
      @?= (Left (EBadArg "Step size cant be 0"),[]),

   testCase "apply print from assignment" $
    runComp (apply "print" [IntVal 42, StringVal "foo", ListVal [TrueVal, ListVal []], IntVal (-1)]) [] -- from the assignment
      @?= (Right NoneVal,["42 foo [True, []] -1"]),
   testCase "apply print from assignment" $
    runComp (apply "print" [NoneVal]) [] -- from the assignment
      @?= (Right NoneVal,["None"]),
   testCase "apply fgdfjg from assignment" $
    runComp (apply "fgdfjg" [NoneVal]) []
      @?= (Left (EBadFun "fgdfjg"),[])]