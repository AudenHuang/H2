module Expr where

import Data.Either

import Parsing

type Name = String

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | ToString Expr
          | Val Int
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit
  deriving Show

data Value = IntVal Int| FloatVal Float| StrVal String| BoolVal Bool|Null
  deriving Eq


data Error = Nothing -- Not yet define

instance Show Value where
  show (IntVal i)      = show i
  show (FloatVal f)    = show f
  show (StrVal s)      = show s
  show (BoolVal b)     = show b
  show Null            = "NULL"

data BinTree = Leaf | Node (Name, Value) BinTree BinTree

instance Show BinTree where
  show bt = show (showTree bt)
  
showTree :: BinTree -> [(Name, Value)]
showTree Leaf                       = []
showTree (Node (name, value) lt rt) = showTree lt ++ [(name, value)] ++ showTree rt

eval :: BinTree -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Either Error Value -- Result (if no errors such as missing variables)
        
-- need to redefine the following functions with bintree
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = Nothing -- return an error (because it's not implemented yet!)
eval vars (ToString x) = Nothing

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pCommand :: Parser Command
pCommand = do t <- letter
              char '='
              e <- pExpr
              return (Set [t] e)
            ||| do string "print"
                   space
                   e <- pExpr
                   return (Print e)

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   error "Subtraction not yet implemented!" 
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- digit
             return (Val (digitToInt d))
           ||| do v <- letter
                  error "Variables not yet implemented" 
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              error "Multiplication not yet implemented" 
            ||| do char '/'
                   t <- pTerm
                   error "Division not yet implemented" 
                 ||| return f

pQuit :: Parser Command
pQuit = do string "quit"
           return Quit