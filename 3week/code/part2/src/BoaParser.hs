-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where
import BoaAST
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.Char

-- Constants used later.
stringChar = '\''
listStartChar = '['
listEndChar = ']'

-- list of keywords used for checking up against later.
keywords :: [String]
keywords = ["None", "True", "False", "for", "if", "in", "not"]

-- main loop simply calls the parse functions on a string of
-- statements (s) and parses it through stmts as the entrance
-- parser to the rest of the parsers.   
parseString :: String -> Either ParseError Program
parseString s = do  p <- parse stmts "Error" s
                    return p

-- a function which uses the haskell native function any to
-- check if "a" is in a list of "a"'s
isInList :: Eq a => a -> [a] -> Bool
isInList s list = any (\k -> s == k ) list


-- parses the list of statments that makes up a program.                                    
stmts :: GenParser Char st [Stmt]
stmts = do  s1 <- stmt
            spaces
            stmtl <- stmtsL s1
            spaces
            eof
            return stmtl

--aux. function, it is a factorized verison of stmts.
stmtsL :: Stmt -> GenParser Char st [Stmt]
stmtsL s = do   spaces
                char ';'
                spaces
                s2 <- stmts
                return (s:s2)
            <|> return [s]

-- a single statement (e.g. a single line seperated by ;)
stmt :: GenParser Char st Stmt
stmt = try (
    do  id <- identString
        spaces
        char '='
        spaces
        e1 <- expr
        spaces
        return (SDef id e1))
    <|> do  e1 <- expr
            return (SExp e1)

--Our term function is not really a term as in terminal,
--But rather the function that checks for everything else than
--expressions. 
term :: GenParser Char st Exp
term = try notExp
    <|> try list
    <|> try listComprehension
    <|> try parenthesis
    <|> try ident
    <|> try stringConst
    <|> try numConst
    <|> try noneConst
    <|> try trueConst
    <|> try falseConst
    <|> do comment
           expr

--the top of the expr function stack. the parsing "entrance" to the lower
--parsing functions, comes from teh program statements into this.
expr :: GenParser Char st Exp
expr =  try (do spaces
                t1 <- term
                multOp t1)
     <|> term

--part of the parsing that handles + and -, works perfectly, even with paranthesis.
--it makes a call to multOp s.t. we can handle all cases. the internal structure
--of both this and multOp and their connection should be taken up for revision.
addOp :: Exp -> GenParser Char st Exp
addOp e = try (do       op <- operator
                        spaces
                        e2 <- term
                        multOp (Oper op e e2))
        <|> return e

--should have been the parser for higher order precedens, but ended up being the 
--main parser which holds most else. THe equality statements should have been factorized.
-- works by checking for operators, then calling teh operators and itself to create
-- the left associative presedence (it become too dominant, s.t. we could not remove it again)
-- would probably be the first to be refactored (changed) had we had more time.
multOp :: Exp -> GenParser Char st Exp
multOp e = try (do      op <- operatorHigherPrecedence
                        spaces
                        e2 <- term
                        case e of
                           Oper Times _ _ -> multOp (Oper op e e2)
                           Oper Div _ _ -> multOp (Oper op e e2)
                           Oper Mod _ _ -> multOp (Oper op e e2)
                           Oper opj j1 j2 -> multOp (Oper opj j1 (Oper op j2 e2))
                           _ -> multOp (Oper op e e2) -- if e is term
                )
        <|> try (
                do spaces
                   string "=="
                   spaces
                   e2 <- expr
                   spaces
                   return (Oper Eq e e2)
        )        
        <|> try (
                do spaces
                   string "<="
                   spaces
                   e2 <- expr
                   spaces
                   return (Oper Less e $ Oper Plus e2 $ Const $ IntVal 1)
        )
        <|> try (
                do spaces
                   string ">="
                   spaces
                   e2 <- expr
                   spaces
                   return (Oper Less e2 $ Oper Plus e $ Const $ IntVal 1)
        )
        <|> try (
                do spaces
                   char '<'
                   spaces
                   e2 <- expr
                   spaces
                   return (Oper Less e e2)
        )
        <|> try (
                do spaces
                   char '>'
                   spaces
                   e2 <- expr
                   spaces
                   return (Oper Greater e e2)
        )
        <|> addOp e

-- almost works, does not work with end of file after the comment.
-- e.g. if there is no newline after the comment it does work.
comment :: GenParser Char st ()
comment = do char '#'
             manyTill anyChar (endOfLine)
             return ()

-- list parser. finds '[' and ']' (respectively listStartChar and
-- listEndChar) and uses the exprz to handle the list parsing.
-- teh second case is for empty lists.
list :: GenParser Char st Exp
list = try (do  spaces
                char listStartChar
                spaces
                ez1 <- exprz
                spaces
                char listEndChar
                spaces
                return (List ez1))
      <|> do    spaces
                char listStartChar
                spaces
                char listEndChar
                return (List [])

--first part of list comprehension. Finds the leading [ symbol
--remove spaces, calls the expression and initiates the latter part.
listComprehension :: GenParser Char st Exp
listComprehension = do  char '['
                        spaces
                        e <- expr
                        listComprehensionL e

-- aux. function to parse list comprehension it is the 
--latter part of a list parsing (first being listComprehension)
--makes use of clauses to make the correct structure.
listComprehensionL :: Exp -> GenParser Char st Exp
listComprehensionL e = do   f <- forClause
                            clauses <- clauses [f]
                            char ']'
                            return (Compr e clauses)

--meta function for clauses (both if and for.).
clauses :: [CClause] -> GenParser Char st [CClause]
clauses e = do  spaces
                f <- forClause
                clauses ((++) e [f])
            <|> do  spaces
                    i <- ifClause
                    clauses ((++) e [i])
            <|> return e

--parser for the for clause, returns the for in structure
forClause :: GenParser Char st CClause
forClause = do  string "for"
                space
                id <- identString
                space
                string "in"
                space
                e2 <- expr
                spaces
                return (CCFor id e2)

--parser for the if-clause, returns if and the expression
--which is the guard the guards whatever comes after.
ifClause :: GenParser Char st CClause
ifClause = do   string "if"
                space
                e1 <- expr
                spaces
                return (CCIf e1)        

exprz :: GenParser Char st [Exp]
exprz = exprs
    <|> return []

exprs :: GenParser Char st [Exp]
exprs = do  e1 <- expr
            spaces
            exprsL e1

--
exprsL :: Exp -> GenParser Char st [Exp]
exprsL e =  do  char ','
                spaces
                e2 <- exprs
                return ((++) [e] e2)
        <|> do  return [e]

-- calls a function by its name with the given argument(s)
functionCall :: String -> GenParser Char st Exp
functionCall id = do    space
                        char '('
                        e1 <- exprs
                        char ')'
                        return (Call id e1)


 
ident :: GenParser Char st Exp
ident = do  idenName <- identString
            idenL idenName

--two possibilites for exprs.
idenL :: String -> GenParser Char st Exp
idenL s = try (functionCall s)
        <|> idenVar s

--function using the identString specifically for variables
idenVar :: String -> GenParser Char st Exp
idenVar s = do  spaces
                return (Var s)

--Finds and parses an identifier (variable name). first checks
--that it is a legal identifier (not starting with number),
--and that it adheres to other standards of an identifier
-- (consists of nubmers, letters and underscores.)
identString :: GenParser Char st String
identString = try (do   c <- satisfy (\c -> isLetter c || c == '_')
                        s <- many1 (satisfy (\c -> isAlphaNum c || c == '_' || isDigit c))
                        if isInList ([c] ++ s) keywords then fail "Cannot have keywords as variable names."
                                                        else return ([c] ++ s))
               <|> do  c <- satisfy (\c -> isLetter c || c == '_')
                       return [c]

--meant to handled paranthesis. does not work as intended with higher
--order of presedence, as this should be "dominant" yet is not.
--does work for + and -
parenthesis :: GenParser Char st Exp 
parenthesis = do    spaces
                    char '('
                    spaces
                    e <- expr
                    spaces
                    char ')'
                    spaces
                    return e

-- for passing not expressions. finds the not keyword,
-- then returns Not and the rest of the expression is handled.
-- an aux. function to the expr function stack
notExp :: GenParser Char st Exp
notExp = do spaces
            string "not"
            space
            e2 <- expr
            return (Not e2)

-- for parsing strings. first checks for prefixed spaces
-- then calls helper function to check for potential garbage 
-- in string and handle the string properly. 
stringConst :: GenParser Char st Exp
stringConst = do    spaces
                    char stringChar
                    stringconstIntermediate ""

-- an auxillary function for the stringConst parser. It makes sure
-- that we do not have any non-ascii characters in our strings (isAscii call)
-- and then it makes sure that all the escape characters are handled in an 
-- orderly fashion.
stringconstIntermediate :: String -> GenParser Char st Exp
stringconstIntermediate s = try (do string "\\'"
                                    stringconstIntermediate (s ++ ['\'']))
                        <|> try (do string "\\\n"
                                    stringconstIntermediate (s ++ ""))
                        <|> try (do string "\\n"
                                    stringconstIntermediate (s ++ "\n"))
                        <|> try (do string "\\\\"
                                    stringconstIntermediate (s ++ "\\"))
                        <|> try (do     s1 <- satisfy (\c -> isAscii c && c /= stringChar && c /= '\\')
                                        stringconstIntermediate (s ++ [s1]))
                        <|> do  char stringChar
                                return (Const (StringVal s))

-- for parsing numbers. We check first that the number does not
-- having prefixed 0's (e.g. 007) as this should error.
-- we then satifies that it is a digit, and then remove spaces
-- for then to return the value in its correct form.
numConst :: GenParser Char st Exp
numConst = try ( do     c <- noneOf "0+"
                        e1 <- many1 (satisfy (\c -> isDigit c))
                        spaces
                        return (Const (IntVal (read ((++) [c] e1)))))
        <|> try ( do    char '-'
                        c <- noneOf "0 +"
                        e1 <- many1 (satisfy (\c -> isDigit c))
                        return (Const (IntVal (-(read ((++) [c] e1))))))
        <|> try ( do    c <- noneOf "0+"
                        return (Const (IntVal (read [c]))))
        <|> do  char '0'
                return (Const (IntVal (0)))

-- parsing the none keyword Parser Char st Exp
noneConst = do string "None"
               return (Const (NoneVal))


-- parsing the false keyword returns it in parsed form
falseConst :: GenParser Char st Exp
falseConst = do string "False"
                return (Const (FalseVal))

-- parsing the true keyword returns it in parsed form
trueConst :: GenParser Char st Exp
trueConst = do string "True"
               return (Const (TrueVal))

-- function for teh first precedens operators, 
-- an aux function for the expr function stack.
operator :: GenParser Char st Op
operator = do   char '+'
                spaces
                return Plus
    <|> do  char '-'
            spaces
            return Minus

-- function that parses for higher order of precedens chracters as * 
-- and // to make sure they are parsed before f.x. + and -.
-- an auxillary function to the expr function stack 
operatorHigherPrecedence :: GenParser Char st Op
operatorHigherPrecedence = do   char '*'
                                spaces
                                return Times
    <|> do  string "//"
            spaces
            return Div
    <|> do  char '%'
            spaces
            return Mod
    