module Expr where

import Data.Either

type Name = String
type Msg = String

data Expr = Val Value
          | Var Name
          -- math operation 
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Abs Expr
          | Mod Expr Expr
          | Pow Expr Expr
          -- string concat
          | Concat Expr Expr
          -- boolean operation
          | Compare Expr Expr
          | Eq Expr Expr
          | NE Expr Expr
          | Gt Expr Expr
          | Lt Expr Expr
          | GE Expr Expr
          | LE Expr Expr
          -- function call
          | FuncCallR Name [Expr] --Name is name of function
          | InputExpr
          -- logic operations
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
  deriving (Show, Eq)

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | While Expr [Command]
             | IfE Expr [Command] [Command]
             | If Expr [Command]
             | Func Name [Name] [Command] -- Name -> name of function, [Name] -> Argument variables, [Command] -> Commands in the function
             | FuncCall Name [Expr]
             | Import FilePath
             | Return Expr
             | Expr Expr
             | Quit
  deriving Show

data Error = ErrorExpr Name Msg -- Name refers to the expression that causes the error, Msg is the error msg that will be print when the error occur
  deriving Show

data Value = IntVal Int | FltVal Float | StrVal String | BoolVal Bool | FunCall Name [Expr]| Null | Input
  deriving Eq

instance Show Value where
  show (IntVal i)  = show i
  show (FltVal f)  = show f
  show (StrVal s)  = show s
  show (BoolVal b) = show b
  show Null        = "NULL"
  show Input       = "INPUT"

data BinTree = Leaf | Node (Name, Value) BinTree BinTree

--searching the bintree to find a given variable to see if that variable has been initialised or not
searchBinTree :: Name -> BinTree -> Either Error Value
searchBinTree name' Leaf = Left (ErrorExpr "Var" (name' ++ " hasn't been initialised"))
searchBinTree name' (Node (name, value) binTreeL binTreeR)
  | name' < name = searchBinTree name' binTreeL
  | name' > name = searchBinTree name' binTreeR
  | otherwise    = Right value

-- Evaluating the input
-- Returns a value or an error
eval :: BinTree -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Either Error Value -- Result (if no errors such as missing variables)

-- If the input is a plain value (if it is string, integer, float or an boolean value)
-- Returns the input 
eval vars (Val x)      = Right x -- for values, just give the value directly

-- If the input is a variable search through the binary tree to find its' value
-- Returns the value of that variable
eval vars (Var x)      = searchBinTree x vars

-- If the input is an expr for string concatenation
-- Returns concated string if both input value x and y are a string else returns an error (Left)
eval vars (Concat x y) = case (eval vars x, eval vars y) of
  (Right (StrVal a), Right (StrVal b))    -> Right (StrVal (a ++ b))
  (Right (StrVal a), Right otherVal)      -> Left (ErrorExpr "Concat" (show otherVal ++ " is not a string"))
  (Right (StrVal a), Left undefineValue)  -> Left undefineValue
  (Right otherVal, _)                     -> Left (ErrorExpr "Concat" (show otherVal ++ " is not a string"))
  (Left undefineValue, _)                 -> Left undefineValue

eval vars InputExpr         = Right Input

--Evaluating toString, toInt and toString and return a value in the correct Value "type" if thers's no error else return left
eval vars (FuncCallR name args) = let toString :: [Expr] -> Either Error Value
                                      toString [expr]  = case eval vars expr of
                                                              Right (IntVal i) -> Right (StrVal (show i))
                                                              Right (FltVal f) -> Right (StrVal (show f))
                                                              Right (StrVal s) -> Right (StrVal s)
                                                              _                -> Left (ErrorExpr "toString" (show expr ++ " can't convert to string"))
                                      toInt :: [Expr] -> Either Error Value
                                      toInt [expr]  = case eval vars expr of
                                                           -- we read the string that contains a number into float so it can read both int and float 
                                                           -- then we rounded it up to make sure the result is an int
                                                           Right (StrVal s) -> Right (IntVal (round(read s::Float)))
                                                           Right (FltVal f) -> Right (IntVal (round f))
                                                           Right (IntVal i) -> Right (IntVal i)
                                                           _                ->  Left (ErrorExpr "toInt" (show args ++ " can't convert to int"))
                                      toFlt :: [Expr] -> Either Error Value
                                      toFlt [expr]  = case eval vars expr of
                                                           Right (StrVal s) -> Right (FltVal (read s :: Float))
                                                           Right (IntVal i) -> Right (FltVal (read (show i)::Float))
                                                           Right (FltVal f) -> Right (FltVal f)
                                                           _                ->  Left (ErrorExpr "toFlt" (show args ++ " can't convert to float"))

                                      in case name of
                                              "toString" -> toString args
                                              "toInt"    -> toInt args
                                              "toFloat"  -> toFlt args
                                              _          -> Right (FunCall name args)

-- If the input is an expression for Abs
-- Returns the absolute value of the input value or an error (if the input value isn't a number)
eval vars (Abs x)             = case eval vars x of
                                     Right (IntVal i) -> Right (IntVal (abs i))
                                     Right (FltVal f) -> Right (FltVal (abs f))
                                     _                -> Left (ErrorExpr "Abs" (show x ++ " isn't a number"))
                                     
-- If the input is an expression for Mod, modulo x by y
-- Returns the modulo result (if both x and y are integer) or an error
eval vars (Mod x y)           = case (eval vars x, eval vars y) of
                                     (Right (IntVal i1), Right (IntVal i2)) -> Right (IntVal (mod i1 i2))
                                     (Right (IntVal i), Right otherVal) -> Left (ErrorExpr "Mod" (show otherVal ++ " isn't an integer"))
                                     (Right (IntVal i), Left undefineValue) -> Left undefineValue
                                     (Right otherVal, _) -> Left (ErrorExpr "Mod" (show otherVal ++ " is not an integer"))
                                     (Left undefineValue, _) -> Left undefineValue
-- Put the expression through the right operation
eval vars expr = 
  case expr of
       --mathOP cases
       Add x y    -> mathOP    vars x y "+"
       Sub x y    -> mathOP    vars x y "-"
       Mul x y    -> mathOP    vars x y "*"
       Div x y    -> mathOP    vars x y "/"
       Pow x y    -> mathOP    vars x y "^"
       --boolOp cases
       Eq  x y    -> boolOp    vars x y [EQ]
       NE  x y    -> boolOp    vars x y [LT, GT] 
       Lt  x y    -> boolOp    vars x y [LT]
       Gt  x y    -> boolOp    vars x y [GT]
       LE  x y    -> boolOp    vars x y [LT, EQ] 
       GE  x y    -> boolOp    vars x y [GT, EQ]
       --logical operator
       And x y    -> logicalOp vars x y "&&"
       Or  x y    -> logicalOp vars x y "||"
       --reverseOp case
       Not x      -> nBoolOp   vars expr
       --operations that isn't defined in the code
       undefineOp -> Left (ErrorExpr (show undefineOp) ("Unknown operations: " ++ show undefineOp))


-- Math operations
-- Returns a numeric value or an error
mathOP :: BinTree -> Expr -> Expr -> String -> Either Error Value
mathOP vars x y mOp = let op = case mOp of
                                    "+"  -> (+)
                                    "-"  -> (-)
                                    "*"  -> (*)
                                    "/"  -> (/)
                                    "^"  -> (**)
                       in case (eval vars x, eval vars y) of
                               (Right (FltVal f1), Right  (FltVal f2)) -> Right (FltVal (op f1 f2))
                               (Right (FltVal  f), Right  (IntVal  i)) -> Right (FltVal (op f (fromIntegral i)))
                               (Right (IntVal  i), Right  (FltVal  f)) -> Right (FltVal (op (fromIntegral i) f))
                               (Right (IntVal i1), Right  (IntVal i2)) -> Right (IntVal (round (op (fromIntegral i1) (fromIntegral i2))))
                               (Right (FltVal  f), Right     otherVal) -> Left (ErrorExpr "mathOP" (show otherVal ++ " is not a number"))
                               (Right (IntVal  i), Right     otherVal) -> Left (ErrorExpr "mathOP" (show otherVal ++ " is not a number"))
                               (Right (FltVal  f), Left undefineValue) -> Left undefineValue
                               (Right (IntVal  i), Left undefineValue) -> Left undefineValue
                               (Right otherVal, _)                     -> Left (ErrorExpr "mathOP" (show otherVal ++ " is not a number"))
                               (Left undefineValue, _)                 -> Left undefineValue

-- Boolean operations
-- Returns True or false (a boolean value) or an error
boolOp :: BinTree -> Expr -> Expr -> [Ordering] -> Either Error Value
boolOp vars x y bOp = case (eval vars x, eval vars y) of
                           (Right (StrVal   s1), Right (StrVal  s2)) -> Right (BoolVal (compare s1 s2 `elem` bOp))
                           (Right (FltVal   f1), Right (FltVal  f2)) -> Right (BoolVal (compare f1 f2 `elem` bOp))
                           (Right (FltVal    f), Right (IntVal   i)) -> Right (BoolVal (compare f (fromIntegral i) `elem` bOp))
                           (Right (IntVal    i), Right (FltVal   f)) -> Right (BoolVal (compare (fromIntegral i) f `elem` bOp))
                           (Right (IntVal   i1), Right (IntVal  i2)) -> Right (BoolVal (compare i1 i2 `elem` bOp))
                           (Right (BoolVal  b1), Right (BoolVal b2)) -> Right (BoolVal (compare b1 b2 `elem` bOp))
                           (Right b1, Right b2)                      -> Left (ErrorExpr "boolOp" ("Boolean operations don't work between " ++ show x ++ " and " ++ show y))
                           (Right _, Left undefineValue)             -> Left undefineValue
                           (Left undefineValue, _)                   -> Left undefineValue
                               
-- Reversing the boolean value with !
-- Returns an boolean vallue or an error
nBoolOp :: BinTree -> Expr -> Either Error Value
nBoolOp vars (Not x) = case eval vars x of
                            Right (BoolVal  a) -> Right (BoolVal (not a))
                            Right otherVal     -> Left (ErrorExpr "not" (show otherVal ++ " is not a boolean"))
                            Left undefineValue -> Left undefineValue

-- Logical operations with && and ||
-- Returns a boolean value or an error
logicalOp :: BinTree -> Exp -> Expr -> String -> Either Error Value
logicalOp vars x y lOp = let op = case lOp of
                                       "&&" -> (&&)
                                       "||" -> (||)
                          in case (eval vars x, eval vars y) of
                                  (Right (BoolVal b1), Right (BoolVal b2)) -> Right (BoolVal (op b1 b2) )
                                  (Right b1, Right b2)                     -> Left (ErrorExpr "logicalOp" ("Logical Operations only work between two booleans"))
                                  (Right _, Left undefineValue)            -> Left undefineValue
                                  (Left undefineValue, _)                  -> Left undefineValue
