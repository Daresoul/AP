-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
showExp (Cst x) = "(" ++ show x ++ ")"
showExp (Add exp1 exp2) = "(" ++ showExp exp1 ++ "+" ++ showExp exp2 ++ ")"
showExp (Sub exp1 exp2) = "(" ++ showExp exp1 ++ "-" ++ showExp exp2 ++ ")"
showExp (Mul exp1 exp2) = "(" ++ showExp exp1 ++ "*" ++ showExp exp2 ++ ")"
showExp (Div exp1 exp2) = "(" ++ showExp exp1 ++ "`div`" ++ showExp exp2 ++ ")"
showExp (Pow exp1 exp2) = "(" ++ showExp exp1 ++ "^" ++ showExp exp2 ++ ")"
showExp _ = error "Expression not supported."

evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add exp1 exp2) = evalSimple exp1 + evalSimple exp2
evalSimple (Sub exp1 exp2) = evalSimple exp1 - evalSimple exp2
evalSimple (Mul exp1 exp2) = evalSimple exp1 * evalSimple exp2
evalSimple (Div exp1 exp2) = if evalSimple exp2 == 0 then error "Cannot divide by zero"
                                                     else evalSimple exp1 `div` evalSimple exp2
evalSimple (Pow exp1 exp2) = case evalSimple exp1 of
  x -> case evalSimple exp2 of
    y -> if y >= 0 then if x /= 0 then x^y -- the if x /= 0 is for getting through the lazy evaluation, and making it evaluate the value of x
                                  else 0^y -- this is a side product of the above line we just know now that x is 0
                   else error "Cannot use negative exponents"

evalSimple _ = error "Expression not supported"

extendEnv :: VName -> Integer -> Env -> Env
extendEnv var x env = \_v -> if _v == var then Just x else env _v -- Creates a function link to the already existing env function, such that we always get the newest binding first

evalFull :: Exp -> Env -> Integer
evalFull (Cst x) _ = x
evalFull (Add exp1 exp2) env = evalFull exp1 env + evalFull exp2 env
evalFull (Sub exp1 exp2) env = evalFull exp1 env - evalFull exp2 env
evalFull (Mul exp1 exp2) env = evalFull exp1 env * evalFull exp2 env
evalFull (Div exp1 exp2) env = if evalFull exp2 env == 0 then error "Cannot divide by zero"
                                                           else evalFull exp1 env `div` evalFull exp2 env
evalFull (Pow exp1 exp2) env = case evalFull exp1 env of
  x -> case evalFull exp2 env of
    y -> if y >= 0 then if x /= 0 then x^y -- making it evaluate as above
                                  else 0^y
                   else error "Cannot set minus power"

evalFull (If test yes no) env = if evalFull test env /= 0 then evalFull yes env
                                                                else evalFull no env
evalFull (Var v) env = case env v of
  Just x -> x
  _ -> error "The variable isnt bound to any value."

evalFull (Let var val body) env = evalFull body (extendEnv var (evalFull val env) env)
evalFull (Sum var minexp maxexp body) env = case evalFull minexp env of
  min -> case evalFull maxexp env of
    max -> if min > max then 0
                        else if min == max then evalFull body (extendEnv var max env)
                                           else evalFull body (extendEnv var min env) + evalFull (Sum var (Cst (min+1)) (Cst max) body) (extendEnv var (min+1) env)

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst x) _ = Right x

evalErr (Add exp1 exp2) env = case evalErr exp1 env of -- makes each expression calculation into a variable we then can use, while we also can check for errors
  Right val1 -> case evalErr exp2 env of
    Right val2 -> Right (val1 + val2)
    Left err -> Left err
  Left err -> Left err

evalErr (Sub exp1 exp2) env = case evalErr exp1 env of
  Right val1 -> case evalErr exp2 env of
    Right val2 -> Right (val1 - val2)
    Left err -> Left err
  Left err -> Left err

evalErr (Mul exp1 exp2) env = case evalErr exp1 env of
  Right val1 -> case evalErr exp2 env of
    Right val2 -> Right (val1 * val2)
    Left err -> Left err
  Left err -> Left err

evalErr (Div exp1 exp2) env = case evalErr exp1 env of
  Right val1 -> case evalErr exp2 env of
    Right val2 -> if val2 == 0 then Left EDivZero
                               else Right (val1 `div` val2)
    Left err -> Left err
  Left err -> Left err

evalErr (Pow exp1 exp2) env = case evalErr exp1 env of
  Right val1 -> case evalErr exp2 env of
    Right val2 -> if val2 < 0 then Left ENegPower
                              else Right (val1^val2)
    Left err -> Left err
  Left err -> Left err

evalErr (Var v) env = case env v of
  Just x -> Right x
  _ -> Left (EBadVar v) -- Throwing error if we do not posses that variable in the environment

evalErr (If test yes no) env = case evalErr test env of
  Right val1 -> if val1 /= 0 then case evalErr yes env of
                                      Right val2 -> Right val2
                                      Left err -> Left err 
                                   else case evalErr no env of
                                      Right val2 -> Right val2 
                                      Left err -> Left err
  Left err -> Left err

evalErr (Let var val body) env = case evalErr val env of
  Right bindingVal -> case evalErr body (extendEnv var bindingVal env) of
    Right val1 -> Right val1
    Left err -> Left err
  Left err -> Left err

evalErr (Sum var minexp maxexp body) env = case evalErr minexp env of
  Right min -> case evalErr maxexp env of
    Right max -> if min > max then Right 0
                        else if min == max then evalErr body (extendEnv var max env)
                                           else case evalErr body (extendEnv var min env) of
                                                Right current -> case evalErr (Sum var (Cst (min+1)) (Cst max) body) (extendEnv var (min+1) env) of
                                                  Right future -> Right (current + future)
                                                  Left err -> Left err
                                                Left err -> Left err
    Left err -> Left err
  Left err -> Left err

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
