{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

--searching the bintree to find a given variable to see if that variable has been initiate or not
searchBinTree :: Name -> BinTree -> Either Error Value
searchBinTree name' Leaf = Left (ErrorExpr "Var" (name' ++ " hasn't been initiate"))
searchBinTree name' (Node (name, value) binTreeL binTreeR)
  | name' < name = searchBinTree name' binTreeL
  | name' > name = searchBinTree name' binTreeR
  | otherwise    = Right value


eval :: BinTree -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Either Error Value -- Result (if no errors such as missing variables)
eval vars (Val x)      = Right x -- for values, just give the value directly
eval vars (Var x)      = searchBinTree x vars
-- string concatenatopm
eval vars (Concat x y) = case (eval vars x, eval vars y) of
  (Right (StrVal a), Right (StrVal b))    -> Right (StrVal (a ++ b))
  (Right (StrVal a), Right otherVal)      -> Left (ErrorExpr "Concat" (show otherVal ++ " is not a string"))
  (Right (StrVal a), Left undefineValue)  -> Left undefineValue
  (Right otherVal, _)                     -> Left (ErrorExpr "Concat" (show otherVal ++ " is not a string"))
  (Left undefineValue, _)                 -> Left undefineValue
eval vars InputExpr         = Right Input
eval vars (FuncCallR name args) = let toString :: [Expr] -> Either Error Value
                                      toString [expr]  = case eval vars expr of
                                                              Right (IntVal i) -> Right (StrVal (show i))
                                                              Right (FltVal f) -> Right (StrVal (show f))
                                                              Right (StrVal s) -> Right (StrVal s)
                                                              _                -> Left (ErrorExpr "toString" (show expr ++ " can't convert to string"))
                                      toInt :: [Expr] -> Either Error Value
                                      toInt [expr]  = case eval vars expr of
                                                           Right (StrVal s) -> Right (IntVal (round(read s::Float)))
                                                           Right (FltVal f) -> Right (IntVal (round f))
                                                           Right (IntVal i) -> Right (IntVal i)
                                                           _               ->  Left (ErrorExpr "toInt" (show args ++ " can't convert to int"))
                                      toFlt :: [Expr] -> Either Error Value
                                      toFlt [expr]  = case eval vars expr of
                                                           Right (StrVal s) -> Right (FltVal (read s :: Float))
                                                           Right (IntVal i) -> Right (FltVal (read (show i)::Float))
                                                           Right (FltVal f) -> Right (FltVal f)
                                                           _               ->  Left (ErrorExpr "toFlt" (show args ++ " can't convert to float"))

                                      in case name of
                                              "toString" -> toString args
                                              "toInt"    -> toInt args
                                              "toFloat"  -> toFlt args
                                              _          -> Right (FunCall name args)

eval vars (Abs x)             = case eval vars x of
                                     Right (IntVal i) -> Right (IntVal (abs i))
                                     Right (FltVal f) -> Right (FltVal (abs f))
                                     _                -> Left (ErrorExpr "Abs" (show x ++ " is not a float or an integer"))
eval vars (Mod x y)           = case (eval vars x, eval vars y) of
                                     (Right (IntVal a), Right (IntVal b)) -> Right (IntVal (mod a b))
                                     (Right (IntVal a), Right not_int) -> Left (ErrorExpr "Mod" (show not_int ++ " is not an integer"))
                                     (Right (IntVal a), Left undefineValue) -> Left undefineValue
                                     (Right not_int, _) -> Left (ErrorExpr "Mod" (show not_int ++ " is not an integer"))
                                     (Left undefineValue, _) -> Left undefineValue
eval vars expr = 
  case expr of
       --mathOP cases
       Add e e2    -> mathOP vars expr
       Sub e e2    -> mathOP vars expr
       Mul e e2    -> mathOP vars expr
       Div e e2    -> mathOP vars expr
       Pow e e2    -> mathOP vars expr
       --boolOp cases
       Eq e e2     -> boolOp vars expr
       NE e e2     -> boolOp vars expr
       Lt e e2     -> boolOp vars expr
       Gt e e2     -> boolOp vars expr
       LE e e2     -> boolOp vars expr
       GE e e2     -> boolOp vars expr
       --logical operator
       And e e2    -> logicalOp vars expr
       Or  e e2    -> logicalOp vars expr
       --reverseOp case
       Not e       -> reverseBoolOp   vars expr
       --operations that isn't defined in the code
       undefineOp  -> Left (ErrorExpr (show undefineOp) ("Unknown operations: " ++ show undefineOp))


--Math operations
mathOP :: BinTree -> Expr -> Either Error Value
mathOP vars expr = let (func, x, y) = case expr of
                                           Add e1 e2 -> ((+), e1, e2)
                                           Sub e1 e2 -> ((-), e1, e2)
                                           Mul e1 e2 -> ((*), e1, e2)
                                           Div e1 e2 -> ((/), e1, e2)
                                           Pow e1 e2 -> ((**), e1, e2)
                       in case (eval vars x, eval vars y) of
                               (Right (FltVal a), Right (FltVal b)) -> Right (FltVal (func a b))
                               (Right (FltVal f), Right (IntVal i)) -> Right (FltVal (func f (fromIntegral i)))
                               (Right (IntVal i), Right (FltVal f)) -> Right (FltVal (func (fromIntegral i) f))
                               (Right (IntVal a), Right (IntVal b)) -> Right (IntVal (round (func (fromIntegral a) (fromIntegral b))))
                               (Right (FltVal f), Right not_num)    -> Left (ErrorExpr "mathOP" (show not_num ++ " is not a number"))
                               (Right (IntVal i), Right not_num)    -> Left (ErrorExpr "mathOP" (show not_num ++ " is not a number"))
                               (Right (FltVal f), Left undefineValue)    -> Left undefineValue
                               (Right (IntVal f), Left undefineValue)    -> Left undefineValue
                               (Right not_num, _)                   -> Left (ErrorExpr "mathOP" (show not_num ++ " is not a number"))
                               (Left undefineValue, _)                   -> Left undefineValue

-- boolean operations that return True or false
boolOp :: BinTree -> Expr -> Either Error Value
boolOp vars expr = let (ordering, x, y) = case expr of
                                               Lt e1 e2 -> ([LT],  e1, e2)
                                               Gt e1 e2 -> ([GT],  e1, e2)
                                               LE e1 e2 -> ([LT, EQ], e1, e2)
                                               GE e1 e2 -> ([GT, EQ], e1, e2)
                                               Eq e1 e2 -> ([EQ], e1, e2)
                                               NE e1 e2 -> ([LT, GT], e1, e2)
                       in case (eval vars x, eval vars y) of
                               (Right (StrVal  a), Right (StrVal  b)) -> Right (BoolVal (compare a b `elem` ordering))
                               (Right (FltVal  a), Right (FltVal  b)) -> Right (BoolVal (compare a b `elem` ordering))
                               (Right (FltVal  a), Right (IntVal  b)) -> Right (BoolVal (compare a (fromIntegral b) `elem` ordering))
                               (Right (IntVal  a), Right (FltVal  b)) -> Right (BoolVal (compare (fromIntegral a) b `elem` ordering))
                               (Right (IntVal  a), Right (IntVal  b)) -> Right (BoolVal (compare a b `elem` ordering))
                               (Right (BoolVal a), Right (BoolVal b)) -> Right (BoolVal (compare a b `elem` ordering))
                               (Right a, Right b) -> Left (ErrorExpr "boolOp" ("Bool operations between " ++ show x ++ " and " ++ show y ++ " are not supported"))
                               (Right _, Left undefineValue) -> Left undefineValue
                               (Left undefineValue, _) -> Left undefineValue
--Reversing the boolean value with !
reverseBoolOp :: BinTree -> Expr -> Either Error Value
reverseBoolOp vars (Not x) = case eval vars x of
  Right (BoolVal  a) -> Right (BoolVal (not a))
  Right not_bool -> Left (ErrorExpr "not" (show not_bool ++ " is not a boolean"))
  Left undefineValue -> Left undefineValue

--Logical operations with && and || which will also return true or false
logicalOp :: BinTree -> Expr -> Either Error Value
logicalOp vars expr = let (func, x, y) = case expr of
                                              And e1 e2 -> ((&&), e1, e2)
                                              Or  e1 e2 -> ((||), e1, e2)
                          in case (eval vars x, eval vars y) of
                                  (Right (BoolVal a), Right (BoolVal b)) -> Right (BoolVal (func a b) )
                                  (Right a, Right b) -> Left (ErrorExpr "logicalOp" ("Bool operations between " ++ show x ++ " and " ++ show y ++ " are not supported"))
                                  (Right _, Left undefineValue) -> Left undefineValue
                                  (Left undefineValue, _) -> Left undefineValue