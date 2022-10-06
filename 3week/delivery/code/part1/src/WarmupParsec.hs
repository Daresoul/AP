module WarmupParsec where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   <<< fill in here, if different from WarmupReadP >>>

import Text.ParserCombinators.Parsec  -- exports a suitable type ParseError
import Data.Char

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

symbol :: Char -> Parser()
symbol sym = do spaces
                char sym
                spaces
                return ()

nr :: Parser Int
nr = do spaces
        n <- many1 digit
        return (read n)

checkgarbage :: GenParser Char () Char 
checkgarbage = try $ lookAhead $ satisfy (\a -> isDigit a || a == '-' || a == '+')

expE :: Parser Exp
expE = do spaces
          res <- expT
          checkgarbage
          spaces
          expE' res
    <|>do spaces
          symbol '-'
          checkgarbage
          spaces
          res <- expT;
          checkgarbage
          spaces
          expE' (Negate res)

expE' :: Exp -> Parser Exp
expE' res = do spaces
               symbol '+'
               checkgarbage
               spaces
               res2 <- expT
               spaces
               expE' $ Add res res2
         <|>do symbol '-'
               checkgarbage
               spaces
               res2 <- expT
               spaces 
               expE' $ Add res $ Negate res2 -- "-" TE'
         <|>do spaces
               res2 <- expT
               checkgarbage
               spaces
               expE' $ Add res res2
         <|>do return res; -- r

expT :: Parser Exp 
expT = do spaces
          res <- nr
          spaces
          return $ Num res 
    <|>do spaces
          symbol '-'
          spaces
          res <- expT
          return $ Negate res
    <|>do symbol '('
          spaces 
          res<-expE;
          spaces 
          symbol ')'
          return res -- "(" E ")"

parseString :: String -> Either ParseError Exp
parseString input = case parse expE "Error" input of
            Right res -> Right res
            Left err -> Left err