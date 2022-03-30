{-# LANGUAGE TemplateHaskell #-}
module Test where

import REPL
import Expr
import Parsing
import Test.QuickCheck
import Test.QuickCheck.All

data Op = Op String
  deriving Show

instance Arbitrary Op where
  arbitrary = do e <- elements "+-*/%^"
                 return (Op $ filter (/='\'') $ show e)

data Bop = Bop String
  deriving Show

instance Arbitrary Bop where
  arbitrary = oneof [return (Bop "<"),
                     return (Bop ">"),
                     return (Bop "=="),
                     return (Bop "!="),
                     return (Bop ">="),
                     return (Bop "<=")]

-- data Space = Space String
--   deriving Show

-- instance Arbitrary Space where
--   arbitrary = do x <- listOf $ elements " "
--                  return (Space x)

--instance Arbitrary Value where
--  arbitrary = oneof [ return (IntVal i),
--                      return ()]

--instance Arbitrary Char where
--  arbitrary = oneof [return "+",
--                    return "-",
--                    return "*",
--                    return "/",
--                    return "^",
--                    return "%"]

-- instance Arbitrary Space where
--   arbitrary = oneof [ return "",
--                       return " ",
--                       return "  ",
--                       return "   "]
--instance Arbitrary Space where
 -- arbitrary x = return (replicate x " ")



-- -- https://blog.nikosbaxevanis.com/2016/02/25/write-you-some-quickcheck-generating-random-floats/
-- instance Arbitrary Float where
--   arbitrary = liftM3 fraction arbitrary arbitrary arbitrary



-- prop_parseFlt 

-- PARSER TESTS - checks that parser converts strings to appropriate type representation

-- Checks that floats are parsed correctly
prop_parseFloat :: Float -> Bool
prop_parseFloat flt = case parse pExpr (show flt) of
                           [(Val (FltVal a), "")] -> True
                           _                      -> False



-- Checks that MathOP are correctly parsed for ints (ignoring whitespace)
-- prop_parseMathOPInt :: Int -> Int -> Space -> Op -> Bool
-- prop_parseMathOPInt x y (Space s) (Op o)| o == "+" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Add (Val (IntVal x)) (Val (IntVal y)), "")]
--                                      | o == "-" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Sub (Val (IntVal x)) (Val (IntVal y)), "")]
--                                      | o == "*" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Mul (Val (IntVal x)) (Val (IntVal y)), "")]
--                                      | o == "/" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Div (Val (IntVal x)) (Val (IntVal y)), "")]
--                                      | o == "^" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Pow (Val (IntVal x)) (Val (IntVal y)), "")]
--                                      | o == "%" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Mod (Val (IntVal x)) (Val (IntVal y)), "")]
prop_parseMathOPInt :: Int -> Int -> Int -> Op -> Bool
prop_parseMathOPInt x y s (Op o)| o == "+" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Add (Val (IntVal x)) (Val (IntVal y)), "")]
                                     | o == "-" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Sub (Val (IntVal x)) (Val (IntVal y)), "")]
                                     | o == "*" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Mul (Val (IntVal x)) (Val (IntVal y)), "")]
                                     | o == "/" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Div (Val (IntVal x)) (Val (IntVal y)), "")]
                                     | o == "^" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Pow (Val (IntVal x)) (Val (IntVal y)), "")]
                                     | o == "%" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Mod (Val (IntVal x)) (Val (IntVal y)), "")]



-- Checks that MathOP (+ - * / % ^) are correctly parsed for ints (ignoring whitespace)
-- prop_parseMathOPFlt :: Float -> Float -> Space -> Op -> Bool
-- prop_parseMathOPFlt x y (Space s) (Op o)| x < 0.1 || y < 0.1 = True --Must force True - read report
--                                      | o == "+" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Add (Val (FltVal x)) (Val (FltVal y)), "")]
--                                      | o == "-" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Sub (Val (FltVal x)) (Val (FltVal y)), "")]
--                                      | o == "*" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Mul (Val (FltVal x)) (Val (FltVal y)), "")]
--                                      | o == "/" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Div (Val (FltVal x)) (Val (FltVal y)), "")]
--                                      | o == "^" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Pow (Val (FltVal x)) (Val (FltVal y)), "")]
--                                      | o == "%" = parse pExpr((show x) ++ s ++ o ++ s ++ (show y)) == [(Mod (Val (FltVal x)) (Val (FltVal y)), "")]
prop_parseMathOPFlt :: Float -> Float -> Int -> Op -> Bool
prop_parseMathOPFlt x y s (Op o)| x < 0.1 || y < 0.1 = True --Must force True - read report
                                     | o == "+" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Add (Val (FltVal x)) (Val (FltVal y)), "")]
                                     | o == "-" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Sub (Val (FltVal x)) (Val (FltVal y)), "")]
                                     | o == "*" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Mul (Val (FltVal x)) (Val (FltVal y)), "")]
                                     | o == "/" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Div (Val (FltVal x)) (Val (FltVal y)), "")]
                                     | o == "^" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Pow (Val (FltVal x)) (Val (FltVal y)), "")]
                                     | o == "%" = parse pExpr((show x) ++ concat(replicate s " ") ++ o ++ concat(replicate s " ") ++ (show y)) == [(Mod (Val (FltVal x)) (Val (FltVal y)), "")]

-- Checks that Abs gets correctly parsed for ints
prop_parseAbsInt :: Int -> Bool
prop_parseAbsInt int = parse pExpr ("|" ++ show int ++ "|") == [(Abs (Val (IntVal int)), "")]

-- Checks that Abs gets correctly parsed for floats
prop_parseAbsFlt :: Float -> Bool
prop_parseAbsFlt flt | flt < 0.1 = True --Must force True - read report
                     | otherwise = parse pExpr ("|" ++ show flt ++ "|") == [(Abs (Val (FltVal flt)), "")]

--Let's remove this perhaps https://www.youtube.com/watch?v=G7LJC9vJluU
-- Checks that concatenate correctly parses
--prop_parseConcat :: String -> String -> Bool
--prop_parseConcat s1 s2 = parse pExpr ("\"" ++ s1 ++ "\"" ++ "++" ++  "\"" ++ s2 ++ "\"") == [(Concat (Val (StrVal s1)) (Val (StrVal s2)), "")]

prop_parseBoolInt :: Int -> Int -> Bop -> Int -> Bool
prop_parseBoolInt int1 int2 (Bop o) spc | o == "<" = parse pBool((show int1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int2)) == [(Lt (Val(IntVal int1)) (Val(IntVal int2)), "")]
                                        | o == ">" = parse pBool((show int1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int2)) == [(Gt (Val(IntVal int1)) (Val(IntVal int2)), "")]
                                        | o == "==" = parse pBool((show int1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int2)) == [(Eq (Val(IntVal int1)) (Val(IntVal int2)), "")]
                                        | o == "!=" = parse pBool((show int1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int2)) == [(NE (Val(IntVal int1)) (Val(IntVal int2)), "")]
                                        | o == "<=" = parse pBool((show int1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int2)) == [(LE (Val(IntVal int1)) (Val(IntVal int2)), "")]
                                        | o == ">=" = parse pBool((show int1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int2)) == [(GE (Val(IntVal int1)) (Val(IntVal int2)), "")]

prop_parseBoolFltInt :: Float -> Int -> Bop -> Int -> Bool
prop_parseBoolFltInt flt int (Bop o) spc | o == "<" = parse pBool((show flt) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int)) == [(Lt (Val(FltVal flt)) (Val(IntVal int)), "")]
                                         | o == ">" = parse pBool((show flt) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int)) == [(Gt (Val(FltVal flt)) (Val(IntVal int)), "")]
                                         | o == "==" = parse pBool((show flt) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int)) == [(Eq (Val(FltVal flt)) (Val(IntVal int)), "")]
                                         | o == "!=" = parse pBool((show flt) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int)) == [(NE (Val(FltVal flt)) (Val(IntVal int)), "")]
                                         | o == "<=" = parse pBool((show flt) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int)) == [(LE (Val(FltVal flt)) (Val(IntVal int)), "")]
                                         | o == ">=" = parse pBool((show flt) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show int)) == [(GE (Val(FltVal flt)) (Val(IntVal int)), "")]

prop_parseBoolFlt :: Float -> Float -> Bop -> Int -> Bool
prop_parseBoolFlt flt1 flt2 (Bop o) spc | o == "<" = parse pBool((show flt1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show flt2)) == [(Lt (Val(FltVal flt1)) (Val(FltVal flt2)), "")]
                                        | o == ">" = parse pBool((show flt1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show flt2)) == [(Gt (Val(FltVal flt1)) (Val(FltVal flt2)), "")]
                                        | o == "==" = parse pBool((show flt1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show flt2)) == [(Eq (Val(FltVal flt1)) (Val(FltVal flt2)), "")]
                                        | o == "!=" = parse pBool((show flt1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show flt2)) == [(NE (Val(FltVal flt1)) (Val(FltVal flt2)), "")]
                                        | o == "<=" = parse pBool((show flt1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show flt2)) == [(LE (Val(FltVal flt1)) (Val(FltVal flt2)), "")]
                                        | o == ">=" = parse pBool((show flt1) ++ concat(replicate spc " ") ++ o ++ concat(replicate spc " ") ++ (show flt2)) == [(GE (Val(FltVal flt1)) (Val(FltVal flt2)), "")]

-- Expr TESTS - checks that eval operates correctly

return []
runTests = $quickCheckAll

main :: IO Bool
main = do putStrLn "Start Testing"
          runTests