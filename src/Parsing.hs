{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Minor changes by Edwin Brady
-}

module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)
import Expr

infixr 5 |||

{-
The monad of parsers
--------------------
-}

newtype Parser a              =  P (String -> [(a,String)])

instance Functor Parser where
   fmap f p = do p' <- p
                 return (f p')

instance Applicative Parser where
   pure = return
   f <*> a = do f' <- f
                a' <- a
                return (f' a')

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance Alternative Parser where
   empty = mzero
   p <|> q = p ||| q

instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

{-
Basic parsers
-------------
-}

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

{-
Choice
------
-}

(|||)                         :: Parser a -> Parser a -> Parser a
p ||| q                       =  p `mplus` q

{-
Derived primitives
------------------
-}

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p ||| return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  ||| nat

parseFloat                    :: Parser Float
parseFloat                    =  do x <- int
                                    char '.'
                                    y <- nat
                                    return (read (show x ++ "." ++ show y))

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()
{-
Ignoring spacing
----------------
-}

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)

float                         :: Parser Float
float                         =  token parseFloat


-- Statements Parser
pStatement :: Parser Command
pStatement = (do pIfE)
             ||| (do pIf)
             ||| (do pWhile)
             ||| (do pQuit)
             ||| (do pSet)
             ||| (do pPrint)
             ||| (do pDef)
             ||| (do pFuncCall)
             ||| (do pReturn)
             ||| (do pSExpr)

-- Block of statements (if while functions)    
pBlock :: Parser [Command]
pBlock = do symbol "{"
            s <- many pStatement
            symbol "}"
            return s

-- Parsers for different type of statment
-- If-else 
pIfE :: Parser Command
pIfE = do string "if"
          space
          expression <- pOr
          block <- pBlock
          string "else"
          eBlock <- pBlock
          return (IfE expression block eBlock)
-- If
pIf :: Parser Command
pIf = do string "if"
         space
         expression <- pOr
         block <- pBlock
         return (If expression block)

-- While 
pWhile :: Parser Command
pWhile = do string "while"
            space
            expression <- pOr
            space
            block <- pBlock
            return (While expression block)

-- Set variables
pSet :: Parser Command
pSet = do t <- identifier
          symbol "="
          (do e <- pExpr
              return (Set t e)
           ||| do e <- pOr
                  return (Set t e))
-- Quit the program
pQuit :: Parser Command
pQuit = do string "quit"
           return Quit

-- Print statements
pPrint :: Parser Command
pPrint = do string "print"
            space
            (do e <- pExpr
                return (Print e)
             ||| do e <- pOr
                    return (Print e))


-- Return statements
pReturn :: Parser Command
pReturn = do string "return"
             space
             e <- pExpr
             return (Return e)

pSExpr :: Parser Command
pSExpr = (do Expr <$> pOr) ||| (do Expr <$> pExpr)

-- Expr Parsers

pExpr :: Parser Expr
pExpr = (do symbol "input"
            return InputExpr)
        ||| (do t <- pTerm
                do symbol "+"
                   e <- pExpr
                   return (Add t e)
                 ||| do symbol "-"
                        e <- pExpr
                        return (Sub t e)
                     ||| do symbol "++"
                            e <- pExpr
                            return (Concat t e)
                          ||| return t)
--Absolute value
pAbs :: Parser Expr
pAbs = do symbol "|"
          e <- pExpr
          symbol "|"
          return (Abs e)
--Power
pPower :: Parser Expr
pPower = do f <- pFactor
            do symbol "^"
               p <- pPower
               return (Pow f p)
             ||| return f

pTerm :: Parser Expr
pTerm = do f <- pPower
           do symbol "*"
              t <- pTerm
              return (Mul f t)
            ||| do symbol "/"
                   t <- pTerm
                   return (Div f t)
                 ||| do symbol "%"
                        t <- pTerm
                        return (Mod f t)
                      ||| return f

pFactor :: Parser Expr
pFactor = do f <- pFuncCallR
             return f
          ||| do f <- float
                 return (Val (FltVal f))
              ||| do i <- integer
                     return (Val (IntVal i))
                  ||| do v <- identifier
                         return (Var v)
                      ||| do a <- pAbs
                             return a
                          ||| do symbol "("
                                 e <- pExpr
                                 symbol ")"
                                 return e
                               ||| do s <- pString
                                      return s


-- Boolean parser
pBool       :: Parser Expr
pBool       = (do e <- pExpr
                  symbol "<"
                  e2 <- pExpr
                  return (Lt e e2))
              ||| (do e <- pExpr
                      symbol ">"
                      e2 <- pExpr
                      return (Gt e e2))
              ||| (do e <- pExpr
                      symbol "=="
                      e2 <- pExpr
                      return (Eq e e2))
              ||| (do e <- pExpr
                      symbol "!="
                      e2 <- pExpr
                      return (NE e e2))
              ||| (do e <- pExpr
                      symbol ">="
                      e2 <- pExpr
                      return (GE e e2))
              ||| (do e <- pExpr
                      symbol "<="
                      e2 <- pExpr
                      return (LE e e2))
              ||| (do e <- identifier
                      return (Var e))
-- ! parser
pNot   :: Parser Expr
pNot   = (do symbol "True"
             return (Val (BoolVal True))
          ||| do symbol "False"
                 return (Val (BoolVal False)))
              ||| (do f <- pBool
                      return f)
              ||| (do symbol "!"
                      f <- pNot
                      return (Not f))
              ||| (do symbol "("
                      f <- pOr
                      symbol ")"
                      return f)
-- && parser
pAnd  :: Parser Expr
pAnd  = do f <- pNot
           (do symbol "&&"
               f2 <- pNot
               return (And f f2))
             ||| return f
-- || parser
pOr   :: Parser Expr
pOr   = do f <- pAnd
           (do symbol "||"
               f2 <- pAnd
               return (Or f f2))
             ||| return f


-- String Parser
pString :: Parser Expr
pString = do c <- char '"' ||| char '\''
             str <- many (sat (/= c))
             char c
             return (Val (StrVal str))

-- FUNCTION PARSER
-- Function Call statement
pFuncCallR :: Parser Expr
pFuncCallR = do name <- identifier
                args <- pArgs
                return (FuncCallR name args)

-- Function without a return statement 
pFuncCall :: Parser Command
pFuncCall = do name <- identifier
               args <- pArgs
               return (FuncCall name args)

--Argument Parser
pArgs :: Parser [Expr]
pArgs = do symbol "("
           i <- (pCSE [])
           return (i)

-- Comma Sepereated Expressions Parser
-- This check wither the endind is a ) or havind a commas before )
pCSE :: [Expr] -> Parser [Expr]
pCSE [] = (do symbol ")"
              return [])
           ||| (do x <- pExpr
                   pCSE (x:[]))
pCSE xs = (do symbol ","
              x <- pExpr
              pCSE (x:xs))
           ||| (do symbol ")"
                   return (reverse xs))


-- Define Function Parsers
pDef :: Parser Command
pDef = do string "def"
          name <- identifier
          symbol "("
          vars <- pCSV []
          commands <- pBlock
          return (Func name vars commands)

-- Comma Sepereated Variables parser
-- This check wither the endind is a ) or havind a commas before )
pCSV :: [Name] -> Parser [Name]
pCSV [] = (do symbol ")"
              return [])
             ||| (do x <- identifier
                     pCSV (x:[]))
pCSV xs = (do symbol ","
              x <- identifier
              pCSV (x:xs))
             ||| (do symbol ")"
                     return (reverse xs))




