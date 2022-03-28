module REPL where

import System.Console.Haskeline
import Expr
import Parsing
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict (get, put, runStateT, StateT)

-- Copied from https://www.reddit.com/r/haskell/comments/1os0yq/haskeline_woes/
-- Author: TheBB
type StateM = StateT LState IO
type InputM = InputT StateM

data LState = LState {vars :: BinTree, cmds :: [String],
                    functions :: [(Name, [Name], [Command])],
                    wrds :: [String]}

initHLList :: [String]
initHLList = ["print", "def", "while", "if", "else", "toFloat(", "toInt(", "toString(", "quit", "True", "False"]

initLState :: LState
initLState = LState Leaf [] [("", [""], [])] initHLList

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> BinTree -> BinTree
updateVars name' value' Leaf = Node (name', value') Leaf Leaf
updateVars name' value' (Node (name, value) binTreeL binTreeR)
  | name' > name = Node (name, value) binTreeL (updateVars name' value' binTreeR)
  | name' < name = Node (name, value) (updateVars name' value' binTreeL) binTreeR
  | otherwise = Node (name, value') binTreeL binTreeR

updateFunctions :: Name -> [Name] -> [Command] -> [(Name, [Name], [Command])] -> [(Name, [Name], [Command])]
updateFunctions name' vars' cmds' [] = [(name', vars', cmds')]
updateFunctions name' vars' cmds' ((name, vars, cmds): funcs)
  | name' == name = (name', vars', cmds') : funcs
  | otherwise = (name, vars, cmds) : updateFunctions name' vars' cmds' funcs

funCallVal :: LState -> Name -> [Expr] -> InputM (Either Error Value)
funCallVal st name exprs = let scope :: LState
                               scope = st
                               func :: [(Name, [Name], [Command])]
                               func = filter (\(x, _, _) -> x == name) (functions st)
                               setVals :: LState -> [Name] -> [Expr] -> InputM LState
                               setVals lstate (v:vs) (expr:es) = do lstate' <- process lstate (Set v expr)
                                                                    setVals lstate' vs es
                               setVals lstate []     []     = return lstate
                               in case func of
                                       [] -> return (Left (ErrorExpr "FuncCall" "The function hasn't been defined"))
                                       [(funcName, varsName, cmds)] -> if length exprs == length varsName && not(stdBlock cmds)
                                                                          then do sState <- setVals scope varsName exprs
                                                                                  (st', expr) <- rtnBlock (sState, Left (ErrorExpr "" "")) cmds
                                                                                  case expr of
                                                                                       Right expression -> return (eval (vars st') expression)
                                                                                       Left (ErrorExpr expr errorMsg) -> return (Left (ErrorExpr expr errorMsg))
                                                                       else return (Left (ErrorExpr "FuncCall" "Check if the function has a return statement and if the function contains the correct numbers of argumet"))

processBlock :: LState -> [Command] -> InputT StateM LState
processBlock st [cmd]   = do process st cmd
processBlock st (cmd: cmds) = do st' <- process st cmd
                                 processBlock st' cmds
processBlock st _           = return st

--block with return statement
rtnBlock :: (LState, Either Error Expr) -> [Command] -> InputT StateM (LState, Either Error Expr)
rtnBlock (st, _) (Return expr: _)   = return (st, Right expr)
rtnBlock (st, _) [cmd]   = do st' <- process st cmd
                              return (st', Left (ErrorExpr "FuncCall" "The function doesn't contain a return statement"))
rtnBlock (st, _) (cmd: cmds) = do st' <- process st cmd
                                  rtnBlock (st', Left (ErrorExpr "" "")) cmds

--standard block
stdBlock :: [Command] -> Bool
stdBlock []            = True
stdBlock (Return x: _) = False

stdBlock (x: xs)       = stdBlock xs
process :: LState -> Command -> InputT StateM LState
process st (Set var expr) =
  do
    case eval (vars st) expr of
         Left (ErrorExpr expr errorMsg) -> do outputStrLn ("Error : " ++ expr ++ ": " ++ errorMsg)
                                              return st 
         Right (FunCall name exprs) -> do val <- funCallVal st name exprs
                                          case val of
                                               Right eval_res -> return st {vars = updateVars var eval_res (vars st)}
                                               Left (ErrorExpr expr errorMsg) -> do outputStrLn ("Error : " ++ expr ++ ": " ++ errorMsg)
                                                                                    return st 
         Right Input -> do inpVal <- getInputLine "Input > "
                           case inpVal of
                                Just input -> return (st {vars = updateVars var (StrVal input) (vars st)})
                                Nothing -> return (st {vars = updateVars var (StrVal "") (vars st)})
         Right eval_res -> do let st' = st {vars = updateVars var eval_res (vars st)}
                                 in return st'

process st (Print expr) =
  do
    case eval (vars st) expr of
         Left (ErrorExpr expr errorMsg) -> do outputStrLn ("Error : " ++ expr ++ ": " ++ errorMsg)
         Right (FunCall name exprs) -> do val <- funCallVal st name exprs
                                          case val of
                                            Left (ErrorExpr expr errorMsg) -> do outputStrLn ("Error : " ++ expr ++ ": " ++ errorMsg)
                                            Right eval_res -> outputStrLn (show eval_res) 
         Right Input -> do inpVal <- getInputLine "Input > "
                           case inpVal of
                                Just input -> do outputStrLn input
                                Nothing -> do outputStrLn ""
         Right eval_res -> do outputStrLn (show eval_res)
    return st

process st (IfE expr t f) = case eval (vars st) expr of
  Right (BoolVal True)  -> do processBlock st t
  Right (BoolVal False) -> do processBlock st f
  _                     -> do outputStrLn "Invalid if conditional"
                              return st

process st (If expr t) = case eval (vars st) expr of
  Right (BoolVal True)  -> do processBlock st t
  Right (BoolVal False) -> do return st
  _                     -> do outputStrLn "Invalid if conditional"
                              return st

process st (While expr block) = let loop :: LState -> [Command] -> Expr -> InputT StateM LState
                                    loop lstate cmds expr = case eval (vars lstate) expr of
                                                                 Right (BoolVal True)  -> do st' <- processBlock lstate cmds
                                                                                             loop st' cmds expr
                                                                 Right (BoolVal False) -> return lstate
                                                                 _                    -> do outputStrLn "Invalid while conditional"
                                                                                            return lstate
                                    in loop st block expr

process st (Func name vars cmds) = return st {functions = updateFunctions name vars cmds (functions st)}

process st (FuncCall name exprs) = let scope :: LState
                                       scope = st
                                       func :: [(Name, [Name], [Command])]
                                       func = filter (\(a, _, _) -> a == name) (functions st)
                                       setVals :: LState -> [Name] -> [Expr] -> InputT StateM LState
                                       setVals lstate (v:vs) (expr:exps) = do lstate' <- process lstate (Set v expr)
                                                                              setVals lstate' vs exps
                                       setVals lstate [][] = return lstate
                                       in case func of
                                               [] -> return st
                                               [(funcName, varsName, cmds)] -> if length exprs == length varsName && stdBlock cmds
                                                                                  then do sState <- setVals scope varsName exprs
                                                                                          processBlock sState cmds
                                                                                          return st
                                                                               else return st

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: InputM ()
repl = do st <- lift get
          input <- case cmds st of
            [] -> getInputLine "> "
            (x:xs) -> do lift $ put st {cmds = xs}
                         return (Just x)
          st <- lift get
          case input of
            Nothing    -> return ()
            Just input ->
                case parse pStatement input of
                    [(cmd, "")] -> 
                      case cmd of
                        (Set var expr) -> do st' <- process st cmd
                                             if var `notElem` wrds st'
                                              then do lift $ put st' {wrds = var : wrds st'}
                                                      repl
                                             else do lift $ put st' {wrds = wrds st'}
                                                     repl
                        Quit -> return()
                        (Func name _ _)    -> do st' <- process st cmd
                                                 if (name ++ "(") `notElem` wrds st
                                                    then do lift $ put st' {wrds = (name ++ "(") : wrds st'}
                                                            repl
                                                 else do lift $ put st' {wrds = wrds st'}
                                                         repl
                        (Expr expr) -> do process st (Print expr)
                                          repl
                        _ -> do st' <- process st cmd
                                lift $ put st'
                                repl
                    _ -> do outputStrLn "Parse error"
                            repl
