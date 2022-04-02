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

dec                           :: Parser String
dec                           =  do xs <- many1 alphanum
                                    return xs

dec2                          :: Parser String 
dec2                          =  do xs <- many1 digit
                                    return xs

-- Parse the integer (numbers until reacing '.') and save it to i
-- Parse the decimal number (numbers after the '.') and save it to d
-- Returnsa string that combined the integer with a '.' and the decmal number
parseFloat                    :: Parser Float
parseFloat                    =  do i <- int
                                    char '.'
                                    d <- dec2
                                    string "e-"
                                    e <- int
                                    return (1 / ((10^abs e)/(read (show i ++ "." ++ d))))
                                 ||| do char '-'
                                        i <- int
                                        char '.'
                                        d <- dec
                                        return (-read (show i ++ "." ++ d))
                                 ||| do i <- int
                                        char '.'
                                        d <- dec
                                        return (read (show i ++ "." ++ d))
                                 

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

-- Use token to remove spaces
float                         :: Parser Float
float                         =  token parseFloat


-------------------- Commands Parser -----------------
pCommand :: Parser Command
pCommand = (do pIfE)
            ||| (do pIf)
            ||| (do pWhile)
            ||| (do pQuit)
            ||| (do pImport)
            ||| (do pSet)
            ||| (do pPrint)
            ||| (do pDef)
            ||| (do pFuncCall)
            ||| (do pReturn)
            ||| (do pSExpr)

-- Block of commands (if while functions)    
pBlock :: Parser [Command]
pBlock = do symbol "{"
            s <- many pCommand
            symbol "}"
            return s

-- Parsers for different type of coomand
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

-- Import 
pImport :: Parser Command
pImport = do string "import"
             space
             ch <- char '"' 
             path <- many (sat (/= ch))
             char ch
             return (Import path)

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

-- Print 
pPrint :: Parser Command
pPrint = do string "print"
            space
            (do e <- pExpr
                return (Print e)
             ||| do e <- pOr
                    return (Print e))


-- Return 
pReturn :: Parser Command
pReturn = do string "return"
             space
             e <- pExpr
             return (Return e)

-- Expression 
pSExpr :: Parser Command
pSExpr = (do Expr <$> pOr) ||| (do Expr <$> pExpr)

-------------------- Expr Parsers -----------------
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
                  ||| do i <- identifier
                         return (Var i)
                      ||| do a <- pAbs
                             return a
                          ||| do symbol "("
                                 e <- pExpr
                                 symbol ")"
                                 return e
                               ||| do s <- pString
                                      return s


-------------------- Boolean Parser -----------------
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
                      symbol "<="
                      e2 <- pExpr
                      return (LE e e2))
              ||| (do e <- pExpr
                      symbol ">="
                      e2 <- pExpr
                      return (GE e e2))

              ||| (do e <- identifier
                      return (Var e))
-- ! Parser
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
-- && Parser
pAnd  :: Parser Expr
pAnd  = do f <- pNot
           (do symbol "&&"
               f2 <- pNot
               return (And f f2))
             ||| return f
-- || Parser
pOr   :: Parser Expr
pOr   = do f <- pAnd
           (do symbol "||"
               f2 <- pAnd
               return (Or f f2))
             ||| return f


-- String Parser
pString :: Parser Expr
pString = do c <- char '"' ||| char '\''
             s <- many (sat (/= c))
             char c
             return (Val (StrVal s))

-------------------- Function Parser -----------------
-- Function call statement
pFuncCallR :: Parser Expr
pFuncCallR = do name <- identifier
                args <- pArgs
                return (FuncCallR name args)

-- Function call without a return statement 
pFuncCall :: Parser Command
pFuncCall = do name <- identifier
               args <- pArgs
               return (FuncCall name args)

--Boolean Expr Parser
pArgs :: Parser [Expr]
pArgs = do symbol "("
           e <- (pCSE [])
           return e

-- Comma Sepereated Expressions Parser
-- This check whether the end is a ) or having a commas before a )
pCSE :: [Expr] -> Parser [Expr]
pCSE [] = (do symbol ")"
              return [])
           ||| (do e <- pExpr
                   pCSE [e])
pCSE es = (do symbol ","
              e <- pExpr
              pCSE (e:es))
           ||| (do symbol ")"
                   return (reverse es))


-- Define Function Parsers
pDef :: Parser Command
pDef = do string "def"
          name <- identifier
          symbol "("
          vars <- pCSV []
          commands <- pBlock
          return (Func name vars commands)

-- Comma Sepereated Variables Parser
-- This check whether the end is a ) or having a commas before a )
pCSV :: [Name] -> Parser [Name]
pCSV [] = (do symbol ")"
              return [])
             ||| (do v <- identifier
                     pCSV [v])
pCSV vs = (do symbol ","
              v <- identifier
              pCSV (v:vs))
             ||| (do symbol ")"
                     return (reverse vs))

