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

          | Compare Expr Expr
          | Eq Expr Expr
          | NE Expr Expr
          | Gt Expr Expr
          | Lt Expr Expr
          | GE Expr Expr
          | LE Expr Expr

          | FuncCallExpr Name [Expr] --Name is name of function
          | InputExpr

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
             | VoidFuncCall Name [Expr]
             | Return Expr
             | Expr Expr
             | Quit
  deriving Show

data Error = ExprErr Name Msg -- Name refers to the expression that causes the error, Msg is the error msg that will be print when the error occur
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

data BTree = Leaf | Node (Name, Value) BTree BTree

searchBinTree :: Name -> BTree -> Either Error Value
searchBinTree _name Leaf = Left (ExprErr "Var" (_name ++ " not found"))
searchBinTree _name (Node (name, value) binTreeL binTreeR)
  | _name < name = searchBinTree _name binTreeL
  | _name > name = searchBinTree _name binTreeR
  | otherwise    = Right value


eval :: BTree -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Either Error Value -- Result (if no errors such as missing variables)
eval vars (Val x)      = Right x -- for values, just give the value directly
eval vars (Var x)      = searchBinTree x vars
-- string concatenatopm
eval vars (Concat x y) = case (eval vars x, eval vars y) of
  (Right (StrVal a), Right (StrVal b)) -> Right (StrVal (a ++ b))
  (Right (StrVal a), Right not_string) -> Left (ExprErr "Concat" (show not_string ++ " is not a string"))
  (Right (StrVal a), Left eval_err)    -> Left eval_err
  (Right not_string, _)                -> Left (ExprErr "Concat" (show not_string ++ " is not a string"))
  (Left eval_err, _)                   -> Left eval_err
eval vars InputExpr         = Right Input
eval vars (FuncCallExpr name args) = let toString :: [Expr] -> Either Error Value
                                         toString [intExpression]  = case eval vars intExpression of
                                                                          Right (IntVal i) -> Right (StrVal (show i))
                                                                          Right (FltVal f) -> Right (StrVal (show f))
                                                                          Right (StrVal s) -> Right (StrVal s)
                                                                          _                -> Left (ExprErr "toString" (show intExpression ++ " cannot be converted to string"))
                                         toInt :: [Expr] -> Either Error Value
                                         toInt [strExpression]  = case eval vars strExpression of
                                                                       Right (StrVal i) -> Right (IntVal (read i :: Int))
                                                                       Right (FltVal i) -> Right (IntVal (round i))
                                                                       _               ->  Left (ExprErr "toInt" (show args ++ " cannot be converted to integer"))
                                         toFlt :: [Expr] -> Either Error Value
                                         toFlt [strExpression]  = case eval vars strExpression of
                                                                       Right (StrVal i) -> Right (FltVal (read i :: Float))
                                                                       _               ->  Left (ExprErr "toFlt" (show args ++ " cannot be converted to float"))

                                         in case name of
                                                 "toString" -> toString args
                                                 "toInt"    -> toInt args
                                                 "toFloat"  -> toFlt args
                                                 _          -> Right (FunCall name args)

eval vars (Abs x)             = case eval vars x of
                                     Right (IntVal i) -> Right (IntVal (abs i))
                                     Right (FltVal f) -> Right (FltVal (abs f))
                                     _               -> Left (ExprErr "Abs" (show x ++ " is not a float or an integer"))
eval vars (Mod x y)           = case (eval vars x, eval vars y) of
                                     (Right (IntVal a), Right (IntVal b)) -> Right (IntVal (mod a b))
                                     (Right (IntVal a), Right not_int) -> Left (ExprErr "Mod" (show not_int ++ " is not an integer"))
                                     (Right (IntVal a), Left eval_err) -> Left eval_err
                                     (Right not_int, _) -> Left (ExprErr "Mod" (show not_int ++ " is not an integer"))
                                     (Left eval_err, _) -> Left eval_err
eval vars expr = case expr of
  --math
  Add e e2 -> mathOP vars expr
  Sub e e2 -> mathOP vars expr
  Mul e e2 -> mathOP vars expr
  Div e e2 -> mathOP vars expr
  Pow e e2 -> mathOP vars expr
  --compare
  Eq  e e2 -> boolOp  vars expr
  NE  e e2 -> boolOp  vars expr
  Lt  e e2 -> boolOp  vars expr
  Gt  e e2 -> boolOp  vars expr
  LE e e2 -> boolOp  vars expr
  GE e e2 -> boolOp  vars expr
  --logical operator
  And e e2 -> andorBoolOp     vars expr
  Or  e e2 -> andorBoolOp     vars expr
  Not e    -> notBoolOp       vars expr
  otherOp  -> Left (ExprErr (show otherOp) ("Unknown operations: " ++ show otherOp))


--Math operations
mathOP :: BTree -> Expr -> Either Error Value
mathOP vars expr = case (eval vars x, eval vars y) of
  (Right (FltVal a), Right (FltVal b)) -> Right (FltVal (func a b))
  (Right (FltVal f), Right (IntVal i)) -> Right (FltVal (func f (fromIntegral i)))
  (Right (IntVal i), Right (FltVal f)) -> Right (FltVal (func (fromIntegral i) f))
  (Right (IntVal a), Right (IntVal b)) -> Right (IntVal (round (func (fromIntegral a) (fromIntegral b))))
  (Right (FltVal f), Right not_num)    -> Left (ExprErr "mathOP" (show not_num ++ " is not a number"))
  (Right (IntVal i), Right not_num)    -> Left (ExprErr "mathOP" (show not_num ++ " is not a number"))
  (Right (FltVal f), Left eval_err)    -> Left eval_err
  (Right (IntVal f), Left eval_err)    -> Left eval_err
  (Right not_num, _)                   -> Left (ExprErr "mathOP" (show not_num ++ " is not a number"))
  (Left eval_err, _)                     -> Left eval_err
  where
    (func, x, y) = case expr of
      Add expr1 expr2 -> ((+), expr1, expr2)
      Sub expr1 expr2 -> ((-), expr1, expr2)
      Mul expr1 expr2 -> ((*), expr1, expr2)
      Div expr1 expr2 -> ((/), expr1, expr2)
      Pow expr1 expr2 -> ((**), expr1, expr2)

boolOp :: BTree -> Expr -> Either Error Value
boolOp vars expr = case (eval vars x, eval vars y) of
  (Right (StrVal  a), Right (StrVal  b)) -> Right (BoolVal (compare a b `elem` ordering))
  (Right (FltVal  a), Right (FltVal  b)) -> Right (BoolVal (compare a b `elem` ordering))
  (Right (FltVal  a), Right (IntVal  b)) -> Right (BoolVal (compare a (fromIntegral b) `elem` ordering))
  (Right (IntVal  a), Right (FltVal  b)) -> Right (BoolVal (compare (fromIntegral a) b `elem` ordering))
  (Right (IntVal  a), Right (IntVal  b)) -> Right (BoolVal (compare a b `elem` ordering))
  (Right (BoolVal a), Right (BoolVal b)) -> Right (BoolVal (compare a b `elem` ordering))
  (Right a, Right b) -> Left (ExprErr "boolOp" ("Bool operations between " ++ show x ++ " and " ++ show y ++ " are not supported"))
  (Right _, Left eval_err) -> Left eval_err
  (Left eval_err, _) -> Left eval_err
  where
    (ordering, x, y) = case expr of
      Lt  expr1 expr2 -> ([LT],  expr1, expr2)
      Gt  expr1 expr2 -> ([GT],  expr1, expr2)
      LE expr1 expr2 -> ([LT, EQ], expr1, expr2)
      GE expr1 expr2 -> ([GT, EQ], expr1, expr2)
      Eq  expr1 expr2 -> ([EQ], expr1, expr2)
      NE  expr1 expr2 -> ([LT, GT], expr1, expr2)

notBoolOp :: BTree -> Expr -> Either Error Value
notBoolOp vars (Not x) = case eval vars x of
  Right (BoolVal  a) -> Right (BoolVal (not a))
  Right not_bool -> Left (ExprErr "not" (show not_bool ++ " is not a boolean"))
  Left eval_err -> Left eval_err

andorBoolOp :: BTree -> Expr -> Either Error Value
andorBoolOp vars expr = case (eval vars x, eval vars y) of
  (Right (BoolVal a), Right (BoolVal b)) -> Right (BoolVal (func a b) )
  (Right a, Right b) -> Left (ExprErr "andorBoolOp" ("Bool operations between " ++ show x ++ " and " ++ show y ++ " are not supported"))
  (Right _, Left eval_err) -> Left eval_err
  (Left eval_err, _) -> Left eval_err
  where
    (func, x, y) = case expr of
      And expr1 expr2 -> ((&&), expr1, expr2)
      Or  expr1 expr2 -> ((||), expr1, expr2)

