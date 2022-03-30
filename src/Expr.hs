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

--evaluating toString, toInt and toString and return a value in the correct Value "type" if thers's no error else return left
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

-- If the input is an expression for Abs
-- Returns the absolute value of the input value or an error (if the input value isn't a number)
eval vars (Abs x)             = case eval vars x of
                                     Right (IntVal i) -> Right (IntVal (abs i))
                                     Right (FltVal f) -> Right (FltVal (abs f))
                                     _                -> Left (ErrorExpr "Abs" (show x ++ " isn't a number"))
                                     
-- If the input is an expression for Mod, modulo x by y
-- Returns the modulo result (if both x and y are integer) or an error
eval vars (Mod x y)           = case (eval vars x, eval vars y) of
                                     (Right (IntVal a), Right (IntVal b)) -> Right (IntVal (mod a b))
                                     (Right (IntVal a), Right not_int) -> Left (ErrorExpr "Mod" (show not_int ++ " isn't an integer"))
                                     (Right (IntVal a), Left undefineValue) -> Left undefineValue
                                     (Right not_int, _) -> Left (ErrorExpr "Mod" (show not_int ++ " is not an integer"))
                                     (Left undefineValue, _) -> Left undefineValue
-- Put the expression through the right operation
eval vars expr = 
  case expr of
       --mathOP cases
       Add e e2    -> mathOP vars (+) e e2
       Sub e e2    -> mathOP vars (-) e e2
       Mul e e2    -> mathOP vars (*) e e2
       Div e e2    -> mathOP vars (/) e e2
       Pow e e2    -> mathOP vars (**) e e2
       --boolOp cases
      --  Eq e e2     -> boolOp vars EQ EQ e e2
      --  NE e e2     -> boolOp vars LT GT e e2
      --  Lt e e2     -> boolOp vars LT LT e e2
      --  Gt e e2     -> boolOp vars GT GT e e2
      --  LE e e2     -> boolOp vars LT EQ e e2
      --  GE e e2     -> boolOp vars GT EQ e e2
       Eq e e2     -> boolOp vars [EQ] e e2
       NE e e2     -> boolOp vars [LT, GT] e e2
       Lt e e2     -> boolOp vars [LT] e e2
       Gt e e2     -> boolOp vars [GT] e e2
       LE e e2     -> boolOp vars [LT, EQ] e e2
       GE e e2     -> boolOp vars [GT, EQ] e e2
       --logical operator
       And e e2    -> logicalOp vars (&&) e e2
       Or  e e2    -> logicalOp vars (||) e e2
       --reverseOp case
       Not e       -> reverseBoolOp   vars expr
       --operations that isn't defined in the code
       undefineOp  -> Left (ErrorExpr (show undefineOp) ("Unknown operations: " ++ show undefineOp))


-- Math operations
-- Returns a numeric value or an error
mathOP :: BinTree -> Operator -> Expr -> Expr -> Either Error Value
mathOP vars mOp x y = case (eval vars x, eval vars y) of
                           (Right (FltVal f1), Right (FltVal f2) ) -> Right (FltVal (mOp f1 f2))
                           (Right (FltVal f ), Right (IntVal i ) ) -> Right (FltVal (mOp f (fromIntegral i)))
                           (Right (IntVal i ), Right (FltVal f ) ) -> Right (FltVal (mOp (fromIntegral i) f))
                           (Right (IntVal i1), Right (IntVal i2) ) -> Right (IntVal (round (mOp (fromIntegral i1) (fromIntegral i2))))
                           (Right (FltVal f ), Right notNum      ) -> Left (ErrorExpr "mathOP" (show notNum ++ " is not a number"))
                           (Right (IntVal i ), Right notNum      ) -> Left (ErrorExpr "mathOP" (show notNum ++ " is not a number"))
                           (Right (FltVal f ), Left undefineValue) -> Left undefineValue
                           (Right (IntVal i ), Left undefineValue) -> Left undefineValue
                           (Right notNum, _ )                      -> Left (ErrorExpr "mathOP" (show notNum ++ " is not a number"))
                           (Left undefineValue, _)                 -> Left undefineValue

-- Boolean operations
-- Returns True or false (a boolean value) or an error
boolOp :: BinTree -> [Ordering] -> Expr -> Expr -> Either Error Value
boolOp vars bOp x y = case (eval vars x, eval vars y) of
                           (Right (StrVal  s1), Right (StrVal  s2)) -> Right (BoolVal (compare s1 s2 `elem` bOP))
                           (Right (FltVal  f1), Right (FltVal  f2)) -> Right (BoolVal (compare f1 f2 `elem` bOP))
                           (Right (FltVal  f ), Right (IntVal   i)) -> Right (BoolVal (compare f (fromIntegral i) `elem` bOP))
                           (Right (IntVal  i ), Right (FltVal   f)) -> Right (BoolVal (compare (fromIntegral i) f `elem` bOP))
                           (Right (IntVal  i1), Right (IntVal  i2)) -> Right (BoolVal (compare i1 i2 `elem` bOP))
                           (Right (BoolVal b1), Right (BoolVal b2)) -> Right (BoolVal (compare b1 b2 `elem` bOP))
                           (Right b1, Right b2)                     -> Left (ErrorExpr "boolOp" ("Bool operations between " ++ show x ++ " and " ++ show y ++ " are not supported"))
                           (Right _, Left undefineValue) -> Left undefineValue
                           (Left undefineValue, _) -> Left undefineValue
                               
-- Reversing the boolean value with !
-- Returns an boolean vallue or an error
reverseBoolOp :: BinTree -> Expr -> Either Error Value
reverseBoolOp vars (Not x) = case eval vars x of
  Right (BoolVal  a) -> Right (BoolVal (not a))
  Right not_bool -> Left (ErrorExpr "not" (show not_bool ++ " is not a boolean"))
  Left undefineValue -> Left undefineValue

-- Logical operations with && and ||
-- Returns a boolean value or an error
logicalOp :: BinTree -> Operator -> Expr -> Expr -> Either Error Value
logicalOp vars lOp x y =     case (eval vars x, eval vars y) of
                                  (Right (BoolVal a), Right (BoolVal b)) -> Right (BoolVal (lOp a b) )
                                  (Right _, Left undefineValue) -> Left undefineValue
                                  (Left undefineValue, _) -> Left undefineValue
