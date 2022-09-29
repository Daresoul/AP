module WarmupReadP where

-- Original grammar (E is start symbol):
--   Exp  ::= E "+" T | E "-" T | T | "-" T .
--   Exp  ::= Exp a1  | Exp a2  | b1| b2
--   Term ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   Exp  ::= T Exp' | "-" T
--   Exp' ::= "+" T Exp' | "-" T Exp' | empty
--   Term ::= num | "(" E ")"

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

pExp :: Parser Exp
pExp = do e <- pTerm; skipSpaces; e2 <- pExp' e; return e2

pExp' :: Exp -> Parser Exp
pExp' e = do skipSpaces; op <- pAddOp; skipSpaces; e2 <- pTerm; pExp' (op e e2)
          <++ return e

pAddOp :: Parser (Exp -> Exp -> Exp)
pAddOp = do satisfy (\c -> c == '+'); return Add

pTerm :: Parser Exp
pTerm = do satisfy (\c -> c == '('); skipSpaces; e <- pExp; skipSpaces; satisfy (\c -> c == ')'); return e
        +++ do satisfy (\c -> c == '-'); skipSpaces; e <- pTerm; pExp' (Negate e)
        <++ do skipSpaces; s <- munch (\c -> isNumber c); return (Num (read s :: Int))

parseString :: String -> Either ParseError Exp
parseString s = case readP_to_S (do res <- pExp; eof; return res) s of
  [(a, _)] -> Right a
  [] -> Left "Error empty list."
  _ -> Left "Error in parsing."
    