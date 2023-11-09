-- Put yor parser implementation in this file
module ParserImpl where

import Definitions
import Data.Char
import Text.ParserCombinators.Parsec  -- exports a suitable type ParseError
import Text.Parsec.Char

type Preamble = String
type ParseError = String

-- used from an earlier assignment done with Mikkel 
spaceTab :: GenParser Char st ()
spaceTab = try (do      tab
                        spaceTab)
        <|> try (do     endOfLine
                        spaceTab)
        <|> try (do     space
                        spaceTab)
        <|> do  spaces
                return ()

-- the exported funtion to create the start of the parser
parseSpec :: String -> EM (Preamble, EGrammar)
parseSpec s = do        case parse (spec "") "Error" s of
                            Left x -> Left (show x)
                            Right x -> return x

spec :: String -> GenParser Char st (Preamble, EGrammar)
spec s = do string "---" -- waiting for a string starting with the ---
            char '\n' -- expecting a newline right after
            spaceTab
            e <- eRules
            spaceTab
            eof
            return (s, e)
    <|> do  c <- anyChar -- reading the preamble
            s <- spec ((++) s [c])
            return s

-- Parsing eRules
eRules :: GenParser Char st EGrammar
eRules = do e <- eRule
            eRulesL [e]

-- left factoring of the erules
eRulesL :: EGrammar -> GenParser Char st EGrammar
eRulesL e = do  e2 <- eRules
                return ((++) e e2)
        <|> do  return e

-- erule parsing
eRule :: GenParser Char st ERule
eRule = do  lhs <- lHSStart
            return lhs

-- find lhs left hand side with either type or not
lHSStart :: GenParser Char st ERule
lHSStart = try (do  (n, kind) <- lhsName
                    spaceTab
                    opt_type <- optType
                    spaceTab
                    lHSEnd n kind (Just opt_type))
        <|> do  (n, kind) <- lhsName
                spaceTab
                lHSEnd n kind (Nothing)

-- Finding the right hand side ERule
lHSEnd :: String -> RKind -> Maybe Type -> GenParser Char st ERule
lHSEnd n k t = do   string "::="
                    spaceTab
                    a <- alts
                    char '.'
                    spaceTab
                    return ((n, k, t), a)

-- finding the type of the rule
optType :: GenParser Char st Type
optType = do    string "{:"
                spaceTab
                s <- many1 (satisfy (\c -> c /= '}'))
                spaceTab
                char '}'
                spaceTab
                return (AUser s)

-- for finding the alts in the rule
alts :: GenParser Char st ERHS
alts = do   erhs <- seqe
            spaceTab
            altsL erhs

-- left factoring of the alts rule
altsL :: ERHS -> GenParser Char st ERHS
altsL erhs = do     char '|'
                    erhs2 <- seqe
                    altsL (EBar erhs erhs2)
        <|> do  return erhs

-- finding the different sequences of the 
seqe :: GenParser Char st ERHS
seqe = try (do  simz <- simplez
                char '{'
                h <- htext
                char '}'
                return (ESeq simz h))
        <|> do  sim <- simple
                return sim

-- simplez parser 
simplez :: GenParser Char st [ERHS]
simplez = try (do   sim <- simple
                    spaceTab
                    simz <- simplez
                    spaceTab
                    return ((++) [sim] simz))
        <|> do  return []

-- Simple with a left factorized version
simple :: GenParser Char st ERHS
simple = do char '!'
            sim0 <- simple0
            return (ENot sim0)
    <|> do  s <- simple0
            simpleL s

-- The other part of the left factorized version of simple
simpleL :: ERHS -> GenParser Char st ERHS
simpleL erhs =  do  char '?'
                    return (EOption erhs)
            <|> do  char '*'
                    return (EMany erhs)
            <|> return erhs

-- Simple0 after a left recursion elimination done on it, so it removes the infinite loop
simple0 :: GenParser Char st ERHS
simple0 = do    l <- atom
                sim0 <- simple0' l
                return sim0

-- The new rule according to the left recursion elimination
simple0' :: ERHS -> GenParser Char st ERHS
simple0' a = try (do    string "{?" -- to parse predicates
                        s <- htext
                        char '}'
                        sim0 <- simple0' a
                        return (EPred sim0 s))
        <|> return a
                
-- Describing
atom :: GenParser Char st ERHS
atom =  try (do c <- charLit                    -- to do charlits
                return (ESimple (SChar c)))
    <|> try (do string "\"@\""
                return (ESimple SAnyChar))
    <|> try (do s <- tokLit
                return (ESimple (SLit s)))
    <|> try (do char '('
                alt <- alts -- nested stuff
                char ')'
                return alt)
    <|> do  n <- name
            return (ESimple (SNTerm n))

charLit :: GenParser Char st Char
charLit = do    char '\''
                c <- anyChar
                char '\''
                return c

tokLit :: GenParser Char st String
tokLit = try ( do   char '\"' -- for a longer sequence of text
                    c <- satisfy (\c -> isPrint c) -- to make sure its a printable character
                    s <- many1 (satisfy (\c -> isPrint c && c /= '\"'))
                    char '\"'
                    return ((++) [c] s))
            <|> do  char '\"' -- checking for a single character (The one above should be enough and this one never reached, if it would pass thih one)
                    c <- satisfy (\c -> isPrint c)
                    char '\"'
                    return [c]

htext :: GenParser Char st String
htext = do  --c <- satisfy (\c -> c /= ':' && c /= '?' && c /= '}')
            s <- many1 (satisfy (\c -> c /= '}'))
            return (s)

name :: GenParser Char st String -- for use throughout the implementation
name = do   c <- satisfy (\c -> isLetter c || c == '_')
            s <- nameEnd c
            return s

-- shared part of the name that both the general and the lhs part uses
nameEnd :: Char -> GenParser Char st String
nameEnd c = try (do s <- many1 (satisfy (\cx -> isAlphaNum cx || cx == '_' || isDigit cx))
                    return ([c] ++ s))
            <|> return [c]

lhsName :: GenParser Char st (String, RKind) -- is used to get the rkind of for the lhs name
lhsName = try (do   c <- char '_'
                    s <- nameEnd c
                    return (s, RSep))
        <|> try (do c <- satisfy (\c -> isLower c)
                    s <- (nameEnd c)
                    return (s, RToken))
        <|> do  s <- name
                return (s, RPlain)
