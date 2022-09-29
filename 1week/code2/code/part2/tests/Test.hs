-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [evalErrPowTestFail, evalErrPowTest2, evalErrPowTest1, evalFullPowTest2, evalFullPowTest1, evalErrDivTest2, evalErrDivTest1, evalErrMulTest3, evalErrMulTest2, evalErrMulTest1, evalErrSubTest2, evalErrSubTest1, evalErrAddTest1, evalFullPowTest2, evalFullPowTest1, evalFullDivTest2, evalFullDivTest1, evalFullMulTest3, evalFullMulTest2, evalFullMulTest1, evalFullSubTest2, evalFullSubTest1, evalFullAddTest1, evalErrIfNotFail, evalErrIfFail, evalErrIfNo, evalErrIfYes, evalErrLet, evalErrLetFail, evalErrSumAdvancedFail, evalErrLet, evalErrLetFail, evalErrSumAdvanced, evalErrSumBasic, evalFullSumAssignment, evalFullSumAdvanced, evalFullSumBasic, evalFullIfComplex, evalFullIfNo, evalFullIfYes, evalFullLet, evalSimplePowTest1, evalSimplePowTest2, evalSimpleAddTest1, evalSimpleSubTest1, evalSimpleSubTest2, evalSimpleMulTest1, evalSimpleMulTest2, evalSimpleMulTest3, evalSimpleDivTest1, evalSimpleDivTest2] where
  -- eval simple test
  evalSimpleAddTest1 = ("evalSimpleAddTest1", evalSimple (Add (Cst 2) (Cst 2)) == 4)

  evalSimpleSubTest1 = ("evalSimpleSubTest1", evalSimple (Sub (Cst 5) (Cst 3)) == 2)
  evalSimpleSubTest2 = ("evalSimpleSubTest2", evalSimple (Sub (Cst 3) (Cst 5)) == -2)

  evalSimpleMulTest1 = ("evalSimpleMulTest1", evalSimple (Mul (Cst 3) (Cst 5)) == 15)
  evalSimpleMulTest2 = ("evalSimpleMulTest2", evalSimple (Mul (Cst (-3)) (Cst (-5))) == 15)
  evalSimpleMulTest3 = ("evalSimpleMulTest3", evalSimple (Mul (Cst (3)) (Cst (-5))) == -15)

  evalSimpleDivTest1 = ("evalSimpleDivTest1", evalSimple (Div (Cst 5) (Cst (-5))) == -1)
  evalSimpleDivTest2 = ("evalSimpleDivTest2", evalSimple (Div (Cst 15) (Cst 5)) == 3)

  evalSimplePowTest1 = ("evalSimplePowTest1", evalSimple (Pow (Cst 5) (Cst 2)) == 25)
  evalSimplePowTest2 = ("evalSimplePowTest2", evalSimple (Pow (Cst 10) (Cst 0)) == 1)




  evalFullAddTest1 = ("evalFullAddTest1", evalFull (Add (Cst 2) (Cst 2)) initEnv == 4)

  evalFullSubTest1 = ("evalFullSubTest1", evalFull (Sub (Cst 5) (Cst 3)) initEnv == 2)
  evalFullSubTest2 = ("evalFullSubTest2", evalFull (Sub (Cst 3) (Cst 5)) initEnv == -2)

  evalFullMulTest1 = ("evalFullMulTest1", evalFull (Mul (Cst 3) (Cst 5)) initEnv == 15)
  evalFullMulTest2 = ("evalFullMulTest2", evalFull (Mul (Cst (-3)) (Cst (-5))) initEnv == 15)
  evalFullMulTest3 = ("evalFullMulTest3", evalFull (Mul (Cst (3)) (Cst (-5))) initEnv == -15)

  evalFullDivTest1 = ("evalFullDivTest1", evalFull (Div (Cst 5) (Cst (-5))) initEnv == -1)
  evalFullDivTest2 = ("evalFullDivTest2", evalFull (Div (Cst 15) (Cst 5)) initEnv == 3)

  evalFullPowTest1 = ("evalFullPowTest1", evalFull (Pow (Cst 5) (Cst 2)) initEnv == 25)
  evalFullPowTest2 = ("evalFullPowTest2", evalFull (Pow (Cst 10) (Cst 0)) initEnv == 1)

  evalFullLet = ("evalFullLet", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)

  evalFullIfYes = ("evalFullIfYes", evalFull (If (Cst 3) (Cst 5) (Cst 10)) initEnv == 5)
  evalFullIfNo = ("evalFullIfNo", evalFull (If (Cst 0) (Cst 5) (Cst 10)) initEnv == 10)
  evalFullIfComplex = ("evalFullIfComplex", evalFull (If (Add (Cst (-2)) (Cst 1)) (Add (Sub (Div (Cst 5) (Cst 5)) (Mul (Cst 5) (Cst 5))) (Cst 5)) (Cst 10)) initEnv == -19)

  evalFullSumBasic = ("evalFullSumBasic", evalFull (Sum "z" (Cst 1) (Cst 5) (Var "z")) initEnv == 15)
  evalFullSumAdvanced = ("evalFullSumAdvanced", evalFull (Sum "z" (Cst 5) (Let "a" (Cst 5) (Add (Var "a") (Var "a"))) (Add (Var "z") (Mul (Cst 3) (Var "z")))) initEnv == 180)
  evalFullSumAssignment = ("evalFullSumAssignment", evalFull (Sum "z" (Cst 1) (Add (Cst 2) (Cst 2)) (Mul (Var "z") (Var "z"))) initEnv == 30)

  evalErrSumBasic = ("evalErrSumBasic", evalErr (Sum "z" (Cst 1) (Cst 5) (Var "z")) initEnv == Right 15)
  evalErrSumAdvancedFail = ("evalErrSumAdvancedFail", evalErr (Sum "z" (Cst 5) (Let "a" (Cst 5) (Add (Var "a") (Var "a"))) (Add (Var "z") (Mul (Cst 3) (Var "a")))) initEnv == Left (EBadVar "a"))
  evalErrSumAdvanced = ("evalErrSumAdvanced", evalErr (Sum "z" (Cst 5) (Let "a" (Cst 5) (Add (Var "a") (Var "a"))) (Add (Var "z") (Mul (Cst 3) (Var "z")))) initEnv == Right 180)





  evalErrAddTest1 = ("evalErrAddTest1", evalErr (Add (Cst 2) (Cst 2)) initEnv == Right 4)

  evalErrSubTest1 = ("evalErrSubTest1", evalErr (Sub (Cst 5) (Cst 3)) initEnv == Right 2)
  evalErrSubTest2 = ("evalErrSubTest2", evalErr (Sub (Cst 3) (Cst 5)) initEnv == Right (-2))

  evalErrMulTest1 = ("evalErrMulTest1", evalErr (Mul (Cst 3) (Cst 5)) initEnv == Right 15)
  evalErrMulTest2 = ("evalErrMulTest2", evalErr (Mul (Cst (-3)) (Cst (-5))) initEnv == Right 15)
  evalErrMulTest3 = ("evalErrMulTest3", evalErr (Mul (Cst (3)) (Cst (-5))) initEnv == Right (-15))

  evalErrDivTest1 = ("evalErrDivTest1", evalErr (Div (Cst 5) (Cst (-5))) initEnv == Right (-1))
  evalErrDivTest2 = ("evalErrDivTest2", evalErr (Div (Cst 15) (Cst 5)) initEnv == Right 3)
  evalErrDivTestFail = ("evalErrDivTestFail", evalErr (Div (Cst 15) (Cst 0)) initEnv == Left EDivZero)

  evalErrPowTest1 = ("evalErrPowTest1", evalErr (Pow (Cst 5) (Cst 2)) initEnv == Right 25)
  evalErrPowTest2 = ("evalErrPowTest2", evalErr (Pow (Cst 10) (Cst 0)) initEnv == Right 1)
  evalErrPowTestFail = ("evalErrPowTestFail", evalErr (Pow (Cst 10) (Cst (-1))) initEnv == Left ENegPower)

  evalErrLetFail = ("evalErrLetFail", evalErr (Var "x") initEnv == Left (EBadVar "x"))
  evalErrLet = ("evalErrLet", evalErr (Let "a" (Cst 15) (Var "a")) initEnv == Right 15)

  evalErrIfYes = ("evalErrIfYes", evalErr (If (Cst 3) (Cst 5) (Cst 10)) initEnv == Right 5)
  evalErrIfNo = ("evalErrIfNo", evalErr (If (Cst 0) (Cst 5) (Cst 10)) initEnv == Right 10)

  evalErrIfFail = ("evalErrIfFail", evalErr (If (Cst 0) (Cst 5) (Div (Cst 5) (Cst 0))) initEnv == Left EDivZero)
  evalErrIfNotFail = ("evalErrIfNo", evalErr (If (Cst 1) (Cst 5) (Div (Cst 5) (Cst 0))) initEnv == Right 5)


main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
