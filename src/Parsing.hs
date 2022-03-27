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


-- STATEMENT PARSER
pStatement :: Parser Command
pStatement = (do pIfE)
             ||| (do pIf)
             ||| (do pWhile)
             ||| (do pQuit)
             ||| (do pSet)
             ||| (do pPrint)
             ||| (do pFunc)
             ||| (do pVoidFuncCall)
             ||| (do pReturn)
             ||| (do pExpr2)

-- block of statements (if while functions)    
pBlock :: Parser [Command]
pBlock = do symbol "{"
            s <- many pStatement
            symbol "}"
            return s

--Parsers for different type of statment
-- If-else 
pIfE :: Parser Command
pIfE = do string "if"
          space
          expression <- pBoolOr
          block <- pBlock
          string "else"
          eBlock <- pBlock
          return (IfE expression block eBlock)

pIf :: Parser Command
pIf = do string "if"
         space
         expression <- pBoolOr
         block <- pBlock
         return (If expression block)

-- While 
pWhile :: Parser Command
pWhile = do string "while"
            space
            expression <- pBoolOr
            space
            block <- pBlock
            return (While expression block)

-- Assign statements
pSet :: Parser Command
pSet = do t <- identifier
          symbol "="
          (do e <- pExpr
              return (Set t e)
                 ||| do e <- pBoolOr
                        return (Set t e))
                --        ||| do e <- p
                --               return (Set t e))

pQuit :: Parser Command
pQuit = do string "quit"
           return Quit

-- Print statements
pPrint :: Parser Command
pPrint = do string "print"
            space
            (do e <- pExpr
                return (Print e)
             ||| do e <- pBoolOr
                    return (Print e))


-- Return statements
pReturn :: Parser Command
pReturn = do string "return"
             space
             e <- pExpr
             return (Return e)

-- FUNCTION PARSER
-- Function Call statement
pFuncCall :: Parser Expr
pFuncCall = do name <- identifier
               args <- pFuncCallArgs
               return (FuncCallExpr name args)

pVoidFuncCall :: Parser Command
pVoidFuncCall = do name <- identifier
                   args <- pFuncCallArgs
                   return (VoidFuncCall name args)

pFuncCallArgs :: Parser [Expr]
pFuncCallArgs = do symbol "("
                   i <- (pCSExpressions [])
                   return (i)

-- Comma seperated expressions
pCSExpressions :: [Expr] -> Parser [Expr]
pCSExpressions [] = (do symbol ")"
                        return [])
                       ||| (do i <- pExpr
                               pCSExpressions (i:[]))
pCSExpressions ys = (do symbol ","
                        i <- pExpr
                        pCSExpressions (i:ys))
                       ||| (do symbol ")"
                               return (reverse ys))


-- Function definition statement
pFunc :: Parser Command
pFunc = do string "def"
           name <- identifier
           symbol "("
           vars <- pCSVar []      -- This absorbs the ")"
           commands <- pBlock
           return (Func name vars commands)

pCSVar :: [Name] -> Parser [Name]
pCSVar [] = (do symbol ")"
                return [])
               ||| (do i <- identifier
                       pCSVar (i:[]))
pCSVar ys = (do symbol ","
                i <- identifier
                pCSVar (i:ys))
               ||| (do symbol ")"
                       return (reverse ys))


pExpr2 :: Parser Command
pExpr2 = (do Expr <$> pBoolOr) ||| (do Expr <$> pExpr)

-- EXPRESSION Parsers
-- Numeric/String expressions
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

pFactor :: Parser Expr
pFactor = do f <- pFuncCall
             return f
          ||| do f <- float
                 return (Val (FltVal f))
              ||| do d <- integer
                     return (Val (IntVal d))
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

pAbs :: Parser Expr
pAbs = do symbol "|"
          e <- pExpr
          symbol "|"
          return (Abs e)

pPower :: Parser Expr
pPower = do f <- pFactor
            do symbol "^"
               p <- pPower
               return (Pow f p)
             ||| return f

-- STRING PARSER
pString :: Parser Expr
pString = do ch <- char '"' ||| char '\''
             str <- many (sat (/= ch))
             char ch
             return (Val (StrVal str))


-- Boolean Expressions
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
                      return (Ne e e2))
              ||| (do e <- pExpr
                      symbol ">="
                      e2 <- pExpr
                      return (Gte e e2))
              ||| (do e <- pExpr
                      symbol "<="
                      e2 <- pExpr
                      return (Lte e e2))
              ||| (do e <- identifier
                      return (Var e))

pBoolN   :: Parser Expr
pBoolN   = (do symbol "True"
               return (Val (BoolVal True))
            ||| do symbol "False"
                   return (Val (BoolVal False)))
                ||| (do f <- pBool
                        return f)
                ||| (do symbol "!"
                        f <- pBoolN
                        return (Not f))
                ||| (do symbol "("
                        f <- pBoolOr
                        symbol ")"
                        return f)

pBoolAnd  :: Parser Expr
pBoolAnd  = do f <- pBoolN
               (do symbol "&&"
                   f2 <- pBoolN
                   return (And f f2))
                 ||| return f

pBoolOr   :: Parser Expr
pBoolOr   = do f <- pBoolAnd
               (do symbol "||"
                   f2 <- pBoolAnd
                   return (Or f f2))
                 ||| return f