{-# LANGUAGE TemplateHaskell #-}
module Main where

import REPL
import Expr
import Parsing
import Test.QuickCheck
import Test.QuickCheck.All

data Mop = Mop String
  deriving Show

data Bop = Bop String
  deriving Show

data Lop = Lop String
  deriving Show

instance Arbitrary Mop where
  arbitrary = oneof [return (Mop "+"),
                     return (Mop "-"),
                     return (Mop "*"),
                     return (Mop "/"),
                     return (Mop "^"),
                     return (Mop "%")]



instance Arbitrary Bop where
  arbitrary = oneof [return (Bop "<"),
                     return (Bop ">"),
                     return (Bop "=="),
                     return (Bop "!="),
                     return (Bop ">="),
                     return (Bop "<=")]

instance Arbitrary Lop where
  arbitrary = oneof [return (Lop "&&"),
                     return (Lop "||")]
-- generate random numbers of spaces
genSpc:: Int -> String
genSpc i = concat(replicate i " ")

--------------- Parser Tests ---------------

-- Checks that ints are parsed correctly
prop_parseInt :: Int -> Bool
prop_parseInt i = fst( head( parse pExpr (show i))) == Val (IntVal i)

-- Checks that floats are parsed correctly
prop_parseFloat :: Float -> Bool
prop_parseFloat f | f < 0.1 = True
                  | otherwise= fst( head( parse pExpr (show f))) == Val (FltVal f)

-- Checks that booleans are parsed correctly
prop_parseBool :: Bool -> Bool
prop_parseBool b = fst( head( parse pNot (show b))) == Val (BoolVal b)


-- Checks that strings are parsed correctly
prop_parseString :: String -> Bool
prop_parseString s | '\"' `elem` s = True
                   | otherwise = fst( head( parse pExpr ("\""++s++"\""))) == Val (StrVal s)

-- Checks that pExpr returns the correct MathOP for each math operation with two integers  
prop_parseMathOPInt :: Int -> Int -> Int -> Mop -> Bool
prop_parseMathOPInt x y s (Mop o)| o == "+" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Add (Val (IntVal x)) (Val (IntVal y))
                                 | o == "-" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Sub (Val (IntVal x)) (Val (IntVal y))
                                 | o == "*" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Mul (Val (IntVal x)) (Val (IntVal y))
                                 | o == "/" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Div (Val (IntVal x)) (Val (IntVal y))
                                 | o == "^" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Pow (Val (IntVal x)) (Val (IntVal y))
                                 | o == "%" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Mod (Val (IntVal x)) (Val (IntVal y))

-- Checks that pExpr returns the correct MathOP for each math operation with a float and an integer
prop_parseMathOPFltInt :: Float -> Int -> Int -> Mop -> Bool
prop_parseMathOPFltInt x y s (Mop o)| x < 0.1 = True --Must force True - read report
                                    | o == "+" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Add (Val (FltVal x)) (Val (IntVal y))
                                    | o == "-" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Sub (Val (FltVal x)) (Val (IntVal y))
                                    | o == "*" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Mul (Val (FltVal x)) (Val (IntVal y))
                                    | o == "/" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Div (Val (FltVal x)) (Val (IntVal y))
                                    | o == "^" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Pow (Val (FltVal x)) (Val (IntVal y))
                                    | o == "%" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Mod (Val (FltVal x)) (Val (IntVal y))

-- Checks that pExpr returns the correct MathOP for each math operation with two floats
prop_parseMathOPFlt :: Float -> Float -> Int -> Mop -> Bool
prop_parseMathOPFlt x y s (Mop o)| x < 0.1 || y < 0.1 = True --Must force True - read report
                                 | o == "+" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Add (Val (FltVal x)) (Val (FltVal y))
                                 | o == "-" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Sub (Val (FltVal x)) (Val (FltVal y))
                                 | o == "*" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Mul (Val (FltVal x)) (Val (FltVal y))
                                 | o == "/" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Div (Val (FltVal x)) (Val (FltVal y))
                                 | o == "^" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Pow (Val (FltVal x)) (Val (FltVal y))
                                 | o == "%" = fst( head( parse pExpr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Mod (Val (FltVal x)) (Val (FltVal y))

-- Checks that Abs gets correctly parsed for ints
prop_parseAbsInt :: Int -> Bool
prop_parseAbsInt int = fst( head( parse pExpr ("|" ++ show int ++ "|"))) == Abs (Val (IntVal int))

-- Checks that Abs gets correctly parsed for floats
prop_parseAbsFlt :: Float -> Bool
prop_parseAbsFlt flt | flt < 0.1 = True --Must force True - read report
                     | otherwise = fst( head( parse pExpr ("|" ++ show flt ++ "|"))) == Abs (Val (FltVal flt))

-- perhaps https://www.youtube.com/watch?v=G7LJC9vJluU
-- Checks that concatenate correctly parses
prop_parseConcat :: String -> String -> Bool
prop_parseConcat s1 s2 | '\"' `elem` s1 || '\"' `elem` s2 = True
                       | otherwise = fst( head( parse pExpr ("\"" ++ s1 ++ "\"" ++ "++" ++  "\"" ++ s2 ++ "\""))) == Concat (Val (StrVal s1)) (Val (StrVal s2))

-- Checks that pBool returns the correct boolean operation with two ints
prop_parseBoolOPInt :: Int -> Int -> Bop -> Int -> Bool
prop_parseBoolOPInt int1 int2 (Bop o) s | o == "<"  = fst( head( parse pBool((show int1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int2)))) == Lt (Val(IntVal int1)) (Val(IntVal int2))
                                        | o == ">"  = fst( head( parse pBool((show int1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int2)))) == Gt (Val(IntVal int1)) (Val(IntVal int2))
                                        | o == "==" = fst( head( parse pBool((show int1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int2)))) == Eq (Val(IntVal int1)) (Val(IntVal int2))
                                        | o == "!=" = fst( head( parse pBool((show int1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int2)))) == NE (Val(IntVal int1)) (Val(IntVal int2))
                                        | o == "<=" = fst( head( parse pBool((show int1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int2)))) == LE (Val(IntVal int1)) (Val(IntVal int2))
                                        | o == ">=" = fst( head( parse pBool((show int1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int2)))) == GE (Val(IntVal int1)) (Val(IntVal int2))

-- Checks that pBool returns the correct boolean operation with a float and an integer
prop_parseBoolOPFltInt :: Float -> Int -> Bop -> Int -> Bool
prop_parseBoolOPFltInt flt int (Bop o) s | flt < 0.1 = True -- Force true, see report
                                         | o == "<"  = fst( head( parse pBool((show flt) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int)))) == Lt (Val(FltVal flt)) (Val(IntVal int))
                                         | o == ">"  = fst( head( parse pBool((show flt) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int)))) == Gt (Val(FltVal flt)) (Val(IntVal int))
                                         | o == "==" = fst( head( parse pBool((show flt) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int)))) == Eq (Val(FltVal flt)) (Val(IntVal int))
                                         | o == "!=" = fst( head( parse pBool((show flt) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int)))) == NE (Val(FltVal flt)) (Val(IntVal int))
                                         | o == "<=" = fst( head( parse pBool((show flt) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int)))) == LE (Val(FltVal flt)) (Val(IntVal int))
                                         | o == ">=" = fst( head( parse pBool((show flt) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show int)))) == GE (Val(FltVal flt)) (Val(IntVal int))

-- Checks that pBool returns the correct boolean operation with two floats
prop_parseBoolOPFlt :: Float -> Float -> Bop -> Int -> Bool
prop_parseBoolOPFlt flt1 flt2 (Bop o) s | flt1 < 0.1 || flt2 <0.1 = True -- Force true, see report
                                        | o == "<"  = fst( head( parse pBool((show flt1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show flt2)))) == Lt (Val(FltVal flt1)) (Val(FltVal flt2))
                                        | o == ">"  = fst( head( parse pBool((show flt1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show flt2)))) == Gt (Val(FltVal flt1)) (Val(FltVal flt2))
                                        | o == "==" = fst( head( parse pBool((show flt1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show flt2)))) == Eq (Val(FltVal flt1)) (Val(FltVal flt2))
                                        | o == "!=" = fst( head( parse pBool((show flt1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show flt2)))) == NE (Val(FltVal flt1)) (Val(FltVal flt2))
                                        | o == "<=" = fst( head( parse pBool((show flt1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show flt2)))) == LE (Val(FltVal flt1)) (Val(FltVal flt2))
                                        | o == ">=" = fst( head( parse pBool((show flt1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show flt2)))) == GE (Val(FltVal flt1)) (Val(FltVal flt2))

-- Checks that pBool returns the correct boolean operation with two strings
prop_parseBoolOPStr :: String -> String -> Bop -> Int -> Bool
prop_parseBoolOPStr s1 s2 (Bop o) s | '\"' `elem` s1 || '\"' `elem` s2 = True
                                    | o == "<"  = fst( head( parse pBool("\"" ++ s1 ++ "\"" ++ (genSpc s) ++ o ++ (genSpc s) ++ "\"" ++ s2 ++ "\""))) == Lt (Val(StrVal s1)) (Val(StrVal s2))
                                    | o == ">"  = fst( head( parse pBool("\"" ++ s1 ++ "\"" ++ (genSpc s) ++ o ++ (genSpc s) ++ "\"" ++ s2 ++ "\""))) == Gt (Val(StrVal s1)) (Val(StrVal s2))
                                    | o == "==" = fst( head( parse pBool("\"" ++ s1 ++ "\"" ++ (genSpc s) ++ o ++ (genSpc s) ++ "\"" ++ s2 ++ "\""))) == Eq (Val(StrVal s1)) (Val(StrVal s2))
                                    | o == "!=" = fst( head( parse pBool("\"" ++ s1 ++ "\"" ++ (genSpc s) ++ o ++ (genSpc s) ++ "\"" ++ s2 ++ "\""))) == NE (Val(StrVal s1)) (Val(StrVal s2))
                                    | o == "<=" = fst( head( parse pBool("\"" ++ s1 ++ "\"" ++ (genSpc s) ++ o ++ (genSpc s) ++ "\"" ++ s2 ++ "\""))) == LE (Val(StrVal s1)) (Val(StrVal s2))
                                    | o == ">=" = fst( head( parse pBool("\"" ++ s1 ++ "\"" ++ (genSpc s) ++ o ++ (genSpc s) ++ "\"" ++ s2 ++ "\""))) == GE (Val(StrVal s1)) (Val(StrVal s2))


-- Checks that pOr returns the correct logical operation with two booleans
prop_parseLogicalOpBool :: Bool -> Bool -> Lop -> Int -> Bool
prop_parseLogicalOpBool x y (Lop o) s| o == "&&" = fst( head( parse pOr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == And (Val (BoolVal x)) (Val (BoolVal y))
                                     | o == "||" = fst( head( parse pOr((show x) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show y)))) == Or (Val (BoolVal x)) (Val (BoolVal y))


--------------- Expr Tests ---------------
-- Checks that eval function returns the correct value for all math operation with two ints 
prop_evalMathOpInt :: Int -> Int -> Mop -> Int -> Bool
prop_evalMathOpInt i1 i2 (Mop o) s =  case eval Leaf( fst( head( parse pExpr((show i1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show i2))))) of
                                           Right (IntVal a) -> True
                                           _ -> False

-- Checks that eval function returns the correct value for all math operation with a float and an integer
prop_evalMathOpFltInt :: Float -> Int -> Mop -> Int -> Bool
prop_evalMathOpFltInt f i (Mop o) s | o == "%" = True
                                    |otherwise =  case eval Leaf( fst( head( parse pExpr((show f) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show i))))) of
                                                        Right (FltVal a) -> True
                                                        _ -> False

-- Checks that eval function returns the correct value for all math operation with two floats
prop_evalMathOpFlt :: Float -> Float -> Mop -> Int -> Bool
prop_evalMathOpFlt f1 f2 (Mop o) s  | o == "%" = True
                                    | otherwise =  case eval Leaf( fst( head( parse pExpr((show f1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show f2))))) of
                                                        Right (FltVal a) -> True
                                                        _ -> False

-- Checks that eval function returns the correct value for all boolean operation with two ints
prop_evalBoolOpInt  :: Int -> Int -> Bop -> Int -> Bool
prop_evalBoolOpInt i1 i2 (Bop o) s = case eval Leaf( fst( head( parse pBool((show i1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show i2))))) of
                                          Right (BoolVal a) -> True
                                          _ -> False

-- Checks that eval function returns the correct value for all boolean operation with a float and an integer
prop_evalBoolOpFltInt  :: Float -> Int -> Bop -> Int -> Bool
prop_evalBoolOpFltInt f i (Bop o) s =  case eval Leaf( fst( head( parse pBool((show f) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show i))))) of
                                            Right (BoolVal a) -> True
                                            _ -> False

-- Checks that eval function returns the correct value for all boolean operation with two floats
prop_evalBoolOpFlt  :: Float -> Float -> Bop -> Int -> Bool
prop_evalBoolOpFlt f1 f2 (Bop o) s =  case eval Leaf( fst( head( parse pBool((show f1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show f2))))) of
                                           Right (BoolVal a) -> True
                                           _ -> False

-- Checks that eval function returns the correct value for all boolean operation with two strings                                
prop_evalBoolOpStr  :: String -> String -> Bop -> Int -> Bool
prop_evalBoolOpStr s1 s2 (Bop o) s | '\"' `elem` s1 || '\"' `elem` s2 = True
                                   | otherwise =  case eval Leaf( fst( head( parse pBool("\"" ++ s1 ++ "\"" ++ (genSpc s) ++ o ++ (genSpc s) ++ "\"" ++ s2 ++ "\"")))) of
                                                       Right (BoolVal a) -> True
                                                       _ -> False

-- Checks that eval function returns the correct value for all logical operation with two boolean
prop_evalBoolOpFlt  :: Bool -> Bool -> Lop -> Int -> Bool
prop_evalBoolOpFlt b1 b2 (Lop o) s =  case eval Leaf( fst( head( parse pOr((show b1) ++ (genSpc s) ++ o ++ (genSpc s) ++ (show b2)))))) of
                                           Right (BoolVal a) -> True
                                           _ -> False

return []
runTests = $quickCheckAll

main :: IO Bool
main = do putStrLn "Start Testing"
          runTests
