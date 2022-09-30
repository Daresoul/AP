-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where
import BoaAST
import Text.ParserCombinators.Parsec
import Data.Char

stringChar = '\''
listStartChar = '['
listEndChar = ']'

keywords :: [String]
keywords = ["None", "True", "False", "for", "if", "in", "not"]

parseString :: String -> Either ParseError Program
parseString s = do  p <- parse stmts "Error" s
                    return p

isInList :: Eq a => a -> [a] -> Bool
isInList s list = any (\k -> s == k ) list

                                    
stmts :: GenParser Char st [Stmt]
stmts = do  s1 <- stmt
            spaces
            stmtl <- stmtsL s1
            return stmtl

stmtsL :: Stmt -> GenParser Char st [Stmt]
stmtsL s = do   char ';'
                spaces
                s2 <- stmts
                return (s:s2)
            <|> return [s]

stmt :: GenParser Char st Stmt
stmt = try (
    do  id <- identString
        spaces
        char '='
        spaces
        e1 <- expr
        spaces
        eof
        return (SDef id e1))
    <|> do  e1 <- expr
            eof
            return (SExp e1)

term :: GenParser Char st Exp
term = try notExp
    <|> try ident
    <|> try listComprehension
    <|> try list
    <|> try parenthesis
    <|> try numConst
    <|> try stringConst

expr :: GenParser Char st Exp
expr = do    spaces
             t1 <- term
             multOp t1
     <|> try term

addOp :: Exp -> GenParser Char st Exp
addOp e = try (do       op <- operator
                        spaces
                        e2 <- term
                        multOp (Oper op e e2))
        <|> return e

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
                   return (Oper Greater e2 e)
        )
        <|> try (
                do spaces
                   string ">="
                   spaces
                   e2 <- expr
                   spaces
                   return (Oper Less e2 e)
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
        <|> return e

-- comment :: GenParser Char st ()
-- comment = do char "#"
--              skipMany1 (\c -> endOfLine)

listComprehension :: GenParser Char st Exp
listComprehension = do  char '['
                        spaces
                        e <- expr
                        listComprehensionL e

listComprehensionL :: Exp -> GenParser Char st Exp
listComprehensionL e = do   f <- forClause
                            clauses <- clauses [f]
                            char ']'
                            return (Compr e clauses)

clauses :: [CClause] -> GenParser Char st [CClause]
clauses e = do  spaces
                f <- forClause
                clauses ((++) e [f])
            <|> do  spaces
                    i <- ifClause
                    clauses ((++) e [i])
            <|> return e

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

exprsL :: Exp -> GenParser Char st [Exp]
exprsL e =  do  char ','
                spaces
                e2 <- exprs
                return ((++) [e] e2)
        <|> do  return [e]


functionCall :: String -> GenParser Char st Exp
functionCall id = do    space
                        char '('
                        e1 <- exprs
                        char ')'
                        return (Call id e1)

ident :: GenParser Char st Exp
ident = do  idenName <- identString
            idenL idenName

idenL :: String -> GenParser Char st Exp
idenL s = try (functionCall s)
        <|> idenVar s

idenVar :: String -> GenParser Char st Exp
idenVar s = do  spaces
                return (Var s)

identString :: GenParser Char st String
identString = try (do   c <- satisfy (\c -> isLetter c || c == '_')
                        s <- many1 (satisfy (\c -> isAlphaNum c || c == '_' || isDigit c))
                        if isInList ([c] ++ s) keywords then do return ""
                                                        else return ([c] ++ s))
               <|> do  c <- satisfy (\c -> isLetter c || c == '_')
                       return [c]

list :: GenParser Char st Exp
list = do   spaces
            char listStartChar
            spaces
            ez1 <- exprz
            spaces
            char listEndChar
            spaces
            return (List ez1)

parenthesis :: GenParser Char st Exp
parenthesis = do    spaces
                    char '('
                    spaces
                    e <- expr
                    spaces
                    char ')'
                    spaces
                    return e

notExp :: GenParser Char st Exp
notExp = do spaces
            string "not"
            space
            e2 <- expr
            return (Not e2)

stringConst :: GenParser Char st Exp
stringConst = do    spaces
                    char stringChar
                    stringconstIntermediate ""
                    -- e1 <- many1 (satisfy (\c -> isAscii c && c /= stringChar))

stringconstIntermediate :: String -> GenParser Char st Exp
stringconstIntermediate s = try (do string "\\n"
                                    stringconstIntermediate (s ++ "\n"))
                        <|> try (do string "\\\\"
                                    stringconstIntermediate (s ++ "\\"))
                        <|> try (do string "\\'"
                                    stringconstIntermediate (s ++ ['\'']))
                        <|> try (do s1 <- satisfy (\c -> isAscii c && c /= stringChar && c /= '\\')
                                    stringconstIntermediate (s ++ [s1]))
                        <|> do  char stringChar
                                return (Const (StringVal s))             


numConst :: GenParser Char st Exp
numConst = do   e1 <- many1 (satisfy (\c -> isDigit c))
                spaces
                return (Const (IntVal (read e1)))
        <|> do  char '-'
                e1 <- many1 (satisfy (\c -> isDigit c))
                return (Const (IntVal (-(read e1))))

operator :: GenParser Char st Op
operator = do   char '+'
                spaces
                return Plus
    <|> do  char '-'
            spaces
            return Minus

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
    