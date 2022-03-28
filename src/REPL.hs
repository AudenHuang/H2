module REPL where

import System.Console.Haskeline
import Expr
import Parsing
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict (get, put, runStateT, StateT)

type StateM = StateT LState IO
type InputM = InputT StateM
data LState = LState {vars :: BinTree, cmds :: [String],
                    functions :: [(Name, [Name], [Command])],
                    words :: [String]}

initHLCompletionList :: [String]
initHLCompletionList = ["print", "def", "while", "if", "else", "toFloat(", "toInt(", "toString(", "quit", "True", "False"]

initLState :: LState
initLState = LState Leaf [] [("", [""], [])] initHLCompletionList

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

process :: LState -> Command -> InputT StateM LState
process st (Set var expr) =
  do
    case eval (vars st) expr of
      Left (ExprErr expr errorMsg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ errorMsg)
                                         return st -- error
      Right (FunCall name exprs) -> do val <- funCallVal st name exprs
                                       case val of
                                        Right eval_res -> return st {vars = updateVars var eval_res (vars st)}
                                        Left (ExprErr expr errorMsg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ errorMsg)
                                                                           return st -- error
      Right Input -> do inpVal <- getInputLine "Input > "
                        case inpVal of
                         Just inp -> return (st {vars = updateVars var (StrVal inp) (vars st)})
                         Nothing -> return (st {vars = updateVars var (StrVal "") (vars st)})
      Right eval_res -> do
        let st' = st {vars = updateVars var eval_res (vars st)}
        -- st' should include the variable set to the result of evaluating expr
        return st'
process st (Print expr) =
  do
    case eval (vars st) expr of
         Left (ExprErr expr errorMsg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ errorMsg)
         Right (FunCall name exprs) -> do val <- funCallVal st name exprs
                                          case val of
                                            Left (ExprErr expr errorMsg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ errorMsg)
                                            Right eval_res -> outputStrLn (show eval_res) -- how will it show a string
         Right Input -> do inpVal <- getInputLine "Input > "
                           case inpVal of
                                Just inp -> do outputStrLn inp
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

process st (VoidFuncCall name exprs) = let scope :: LState
                                           scope = st
                                           func :: [(Name, [Name], [Command])]
                                           func = filter (\(a, _, _) -> a == name) (functions st)
                                           assignVals :: LState -> [Name] -> [Expr] -> InputT StateM LState
                                           assignVals lstate (v:vs) (expr:es) = do lstate' <- process lstate (Set v expr)
                                                                                   assignVals lstate' vs es
                                           assignVals lstate []     []     = return lstate
                                       in case func of
                                               [] -> return st
                                               [(fname, vnames, cmds)] -> if length exprs == length vnames && emptyBlock cmds
                                                                             then do sState <- assignVals scope vnames exprs
                                                                                     processBlock sState cmds
                                                                                     return st
                                                                            else return st

processBlock :: LState -> [Command] -> InputT StateM LState
processBlock st [cmd]   = do process st cmd
processBlock st (cmd: cmds) = do st' <- process st cmd
                                 processBlock st' cmds
processBlock st _           = return st

blockReturn :: (LState, Either Error Expr) -> [Command] -> InputT StateM (LState, Either Error Expr)
blockReturn (st, _) (Return expr: _)   = return (st, Right expr)
blockReturn (st, _) [cmd]   = do st' <- process st cmd
                                 return (st', Left (ExprErr "FuncCall" "The function doesn't contain a return statement"))
blockReturn (st, _) (cmd: cmds) = do st' <- process st cmd
                                     blockReturn (st', Left (ExprErr "" "")) cmds

emptyBlock :: [Command] -> Bool
emptyBlock []            = True
emptyBlock (Return x: _) = False
emptyBlock (x: xs)       = emptyBlock xs

funCallVal :: LState -> Name -> [Expr] -> InputT StateM (Either Error Value)
funCallVal st name exprs = let scope :: LState
                               scope = st
                               fun :: [(Name, [Name], [Command])]
                               fun = filter (\(x, _, _) -> x == name) (functions st)
                               assignVals :: LState -> [Name] -> [Expr] -> InputT StateM LState
                               assignVals lstate (v:vs) (expr:es) = do lstate' <- process lstate (Set v expr)
                                                                    assignVals lstate' vs es
                               assignVals lstate []     []     = return lstate
                           in case fun of
                                   [] -> return (Left (ExprErr "FuncCall" "The function hasn't been defined"))
                                   [(fname, vnames, cmds)] -> if length exprs == length vnames && not(emptyBlock cmds)
                                                                 then do sState <- assignVals scope vnames exprs
                                                                         (st', expr) <- blockReturn (sState, Left (ExprErr "" "")) cmds
                                                                         case expr of
                                                                              Right expression -> return (eval (vars st') expression)
                                                                              Left (ExprErr expr errorMsg) -> return (Left (ExprErr expr errorMsg))
                                                                 else return (Left (ExprErr "FuncCall" "Check if the function has a return statement and if the function contains the correct numbers of argumet"))

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: InputM ()
repl = do st <- lift get
          inp <- case cmds st of
            [] -> getInputLine "> "
            (x:xs) -> do lift $ put st {cmds = xs}
                         return (Just x)
          st <- lift get
          case inp of
            Nothing    -> return ()
            Just input ->
                case parse pStatement input of
                    [(cmd, "")] -> -- Must parse entire input
                      case cmd of
                        (Set var expr) -> do st' <- process st cmd
                                          if var `notElem` words st'
                                            then do lift $ put st' {words = var : words st'}
                                                    repl
                                          else do lift $ put st' {words = words st'}
                                                  repl
                        Quit -> return()
                        (Func name _ _)    -> do st' <- process st cmd
                                                 if (name ++ "(") `notElem` words st
                                                  then do lift $ put st' {words = (name ++ "(") : words st'}
                                                          repl
                                                 else do lift $ put st' {words = words st'}
                                                         repl
                        (Expr expr) -> do process st (Print expr)
                                          repl
                        _ -> do st' <- process st cmd
                                lift $ put st'
                                repl
                    _ -> do outputStrLn "Parse error"
                            repl