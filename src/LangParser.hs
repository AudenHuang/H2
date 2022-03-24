module LangParser where

import Parsing
import Expr

-- COMMAND AND EXPRESSION PARSER

-- STATEMENT PARSER
pStatement :: Parser Command
pStatement = (do s <- pIf
                 return (s))
        --      ||| (do s <- pIf2
        --              return (s))
             ||| (do s <- pWhile
                     return (s))
             ||| (do s <- pAssign
                     return (s))
             ||| (do s <- pPrint
                     return (s))
             ||| (do s <- pQuit
                     return (s))
             ||| (do s <- pImport
                     return (s))
             ||| (do s <- pFunc
                     return (s))
             ||| (do s <- pVoidFuncCall
                     return (s))
             ||| (do s <- pReturn
                     return (s))
             ||| (do s <- pExpr_
                     return (s))

-- Block of statements (if while functions)    
pBlock :: Parser [Command]
pBlock = do symbol "{"
            s <- many pStatement
            symbol "}"
            return s

-- If statements
pIf :: Parser Command
pIf = do string "if"
         space
         expression <- pBoolOr
         Block <- pBlock
         string "else"
         eBlock <- pBlock
         return (If expression Block eBlock)

-- pIf2 :: Parser Command
-- pIf2 = do string "if"
--               space
--               expression <- pBoolOr
--               Block <- pBlock
--               return (If2 expression Block)

-- While 
pWhile :: Parser Command
pWhile = do string "while"
            space
            expression <- pBoolOr
            space
            Block <- pBlock
            return (While expression Block)

-- Assign statements
pAssign :: Parser Command
pAssign = do t <- identifier
                     symbol "="
                     (do e <- pExpr
                         return (Set t e)
                       ||| do e <- pBoolOr
                              return (Set t e))
                --        ||| do e <- p
                --               return (Set t e))

-- Print statements
pPrint :: Parser Command
pPrint = do string "print"
            space
            (do e <- pExpr
                return (Print e)
             ||| do e <- pBoolOr
                    return (Print e))

-- Import statements
pImport :: Parser Command
pImport = do string "import"
             space
             ch <- char '"' ||| char '\''
             filepath <- many (sat (/= ch))
             char ch
             return (Import filepath)

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
pFunc = do string "fun"
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

-- For expressions to be printed
pExpr_ :: Parser Command
pExpr_ = (do Expr <$> pBoolOr) ||| (do Expr <$> pExpr)

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
