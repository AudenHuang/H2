module Test where

import REPL
import Expr
import Parsing
import Test.QuickCheck
import Test.QuickCheck.All

instance Arbitrary Value where
  arbitrary = oneof [ return (IntVal i),
                      return ()]

instance Arbitrary MathOp where
  arbitrary = oneof [return "+",
                    return "-",
                    return "*".
                    return "/",
                    return "^",
                    return "%"]

-- instance Arbitraty Space where
--   arbitrary = oneof [ return "",
--                       return " ",
--                       return "  ",
--                       return "   "]
instance Arbitraty Space where
  arbitraryS:: Int
  arbitraryS x= return (replicate x " ")



-- -- https://blog.nikosbaxevanis.com/2016/02/25/write-you-some-quickcheck-generating-random-floats/
-- instance Arbitrary Float where
--   arbitrary = liftM3 fraction arbitrary arbitrary arbitrary



prop_parseFlt 

-- PARSER TESTS - checks that parser converts strings to appropriate type representation

-- Checks that floats are parsed correctly
prop_parseFloat :: Float -> Bool
prop_parseFloat flt = case parse pExpr (show flt) of
                           [(Val (FltVal a), "")] -> True
                           _                      -> False



-- Checks that MathOP are correctly parsed for ints (ignoring whitespace)
prop_parseMathOPInt :: Int -> Int -> Space -> MathOp -> Bool
prop_parseMathOPInt x y (Space s) o| o == "+" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Add x y)
                                     | o == "-" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Sub x y)
                                     | o == "*" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Mul x y)
                                     | o == "/" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Div x y)
                                     | o == "^" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Pow x y)
                                     | o == "%" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Mod x y)



-- Checks that MathOP (+ - * / % ^) are correctly parsed for ints (ignoring whitespace)
prop_parseMathOPFlt :: Float -> Float -> Space -> MathOp -> Bool
prop_parseMathOPFlt x y (Space s) o | o == "+" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Add x y)
                                    | o == "-" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Sub x y)
                                    | o == "*" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Mul x y)
                                    | o == "/" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Div x y)
                                    | o == "^" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Pow x y)
                                    | o == "%" = pExpr((show x) ++ s ++ o ++ s ++ (show y)) == (Mod x y)

-- Checks that Abs gets correctly parsed for ints
prop_parseAbsInt :: Int -> Bool
prop_parseAbsInt int = pExpr ("|" ++ show int ++ "|") == (Abs int)

-- Checks that Abs gets correctly parsed for floats
prop_parseAbsFlt :: Float -> Bool
prop_parseAbsInt flt = pExpr ("|" ++ show int ++ "|") == (Abs flt)

-- Checks that concatenate correctly parses
prop_parseConcat :: String -> String -> Bool
prop_parseConcat s1 s2 = pExpr ("\"" ++ s1 ++ "\"" ++ "++" ++  "\"" ++ s2 ++ "\"") == (Concat s1 s2)


-- Expr TESTS - checks that eval operates correctly

return []
runTests = $quickCheckAll

main :: IO ()