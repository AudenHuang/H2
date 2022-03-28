module REPL where

import System.Console.Haskeline
import Expr
import Parsing
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict (get, put, runStateT, StateT)

type StateM = StateT LState IO
type InputM = InputT StateM
data LState = LState {vars :: BTree, commands :: [String],
                    functions :: [(Name, [Name], [Command])],
                    wordList :: [String]}

initHLCompletionList :: [String]
initHLCompletionList = ["print", "def", "while", "if", "else", "toFloat(", "toInt(", "toString(", "quit", "True", "False"]

initLState :: LState
initLState = LState Leaf [] [("", [""], [])] initHLCompletionList

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> BTree -> BTree
updateVars _name _value Leaf = Node (_name, _value) Leaf Leaf
updateVars _name _value (Node (name, value) ltree rtree)
  | _name > name = Node (name, value) ltree (updateVars _name _value rtree)
  | _name < name = Node (name, value) (updateVars _name _value ltree) rtree
  | otherwise = Node (name, _value) ltree rtree

updateFunctions :: Name -> [Name] -> [Command] -> [(Name, [Name], [Command])] -> [(Name, [Name], [Command])]
updateFunctions _name _vars _commands [] = [(_name, _vars, _commands)]
updateFunctions _name _vars _commands ((name, vars, commands): funcs)
  | _name == name = (_name, _vars, _commands) : funcs
  | otherwise = (name, vars, commands) : updateFunctions _name _vars _commands funcs

process :: LState -> Command -> InputT StateM LState
process st (Set var e) =
  do
    case eval (vars st) e of
      Left (ExprErr expr err_msg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ err_msg)
                                        return st -- error
      Right (FunCall name exprs) -> do val <- funCallVal st name exprs
                                       case val of
                                        Right eval_res -> return st {vars = updateVars var eval_res (vars st)}
                                        Left (ExprErr expr err_msg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ err_msg)
                                                                          return st -- error
      Right Input -> do inpVal <- getInputLine "Input > "
                        case inpVal of
                         Just inp -> return (st {vars = updateVars var (StrVal inp) (vars st)})
                         Nothing -> return (st {vars = updateVars var (StrVal "") (vars st)})
      Right eval_res -> do
        let st' = st {vars = updateVars var eval_res (vars st)}
        -- st' should include the variable set to the result of evaluating e
        return st'
process st (Print e) =
  do
    case eval (vars st) e of
         Left (ExprErr expr err_msg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ err_msg)
         Right (FunCall name exprs) -> do val <- funCallVal st name exprs
                                          case val of
                                            Left (ExprErr expr err_msg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ err_msg)
                                            Right eval_res -> outputStrLn (show eval_res) -- how will it show a string
         Right Input -> do inpVal <- getInputLine "Input > "
                           case inpVal of
                                Just inp -> do outputStrLn inp
                                Nothing -> do outputStrLn ""
         Right eval_res -> do outputStrLn (show eval_res)
    return st

process st (IfE e b1 b2) = case eval (vars st) e of
  Right (BoolVal True)  -> do processBlock st b1
  Right (BoolVal False) -> do processBlock st b2
  _                     -> do outputStrLn "Invalid if conditional"
                              return st
process st (If e b1) = case eval (vars st) e of
  Right (BoolVal True)  -> do processBlock st b1
  Right (BoolVal False) -> do return st
  _                     -> do outputStrLn "Invalid if conditional"
                              return st

process st (While e block) = let loop :: LState -> [Command] -> Expr -> InputT StateM LState
                                 loop lstate cmds expr = case eval (vars lstate) expr of
                                  Right (BoolVal True)  -> do st' <- processBlock lstate cmds
                                                              loop st' cmds expr
                                  Right (BoolVal False) -> return lstate
                                  _                    -> do outputStrLn "Invalid while conditional"
                                                             return lstate
                             in loop st block e

process st (Func name vars commands) = return st {functions = updateFunctions name vars commands (functions st)}

process st (VoidFuncCall name exprs) = let scopedLState :: LState
                                           scopedLState = st
                                           func :: [(Name, [Name], [Command])]
                                           func = filter (\(a, _, _) -> a == name) (functions st)
                                           assignVals :: LState -> [Name] -> [Expr] -> InputT StateM LState
                                           assignVals lstate (v:vs) (e:es) = do lstate' <- process lstate (Set v e)
                                                                                assignVals lstate' vs es
                                           assignVals lstate []     []     = return lstate
                                       in case func of
                                               [] -> return st
                                               [(fname, vnames, commands)] -> if length exprs == length vnames && emptyBlock commands
                                                                                 then do sState <- assignVals scopedLState vnames exprs
                                                                                         processBlock sState commands
                                                                                         return st
                                                                              else return st

processBlock :: LState -> [Command] -> InputT StateM LState
processBlock st [cmd]   = do process st cmd
processBlock st (cmd: cmds) = do st' <- process st cmd
                                 processBlock st' cmds
processBlock st _           = return st

blockReturn :: (LState, Either Error Expr) -> [Command] -> InputT StateM (LState, Either Error Expr)
blockReturn (st, _) (Return e: _)   = return (st, Right e)
blockReturn (st, _) [cmd]   = do st' <- process st cmd
                                 return (st', Left (ExprErr "Function call" "No return statement"))
blockReturn (st, _) (cmd: cmds) = do st' <- process st cmd
                                     blockReturn (st', Left (ExprErr "" "")) cmds

emptyBlock :: [Command] -> Bool
emptyBlock []            = True
emptyBlock (Return x: _) = False
emptyBlock (x: xs)       = emptyBlock xs

funCallVal :: LState -> Name -> [Expr] -> InputT StateM (Either Error Value)
funCallVal st name exprs = let scopedLState :: LState
                               scopedLState = st
                               fun :: [(Name, [Name], [Command])]
                               fun = filter (\(x, _, _) -> x == name) (functions st)
                               assignVals :: LState -> [Name] -> [Expr] -> InputT StateM LState
                               assignVals lstate (v:vs) (e:es) = do lstate' <- process lstate (Set v e)
                                                                    assignVals lstate' vs es
                               assignVals lstate []     []     = return lstate
                           in case fun of
                                   [] -> return (Left (ExprErr "Function call" "No such function"))
                                   [(fname, vnames, commands)] -> if length exprs == length vnames && not(emptyBlock commands)
                                                                     then do sState <- assignVals scopedLState vnames exprs
                                                                             (st', e) <- blockReturn (sState, Left (ExprErr "" "")) commands
                                                                             case e of
                                                                                  Right expression -> return (eval (vars st') expression)
                                                                                  Left (ExprErr expr err_msg) -> return (Left (ExprErr expr err_msg)) -- Should never happen
                                                                  else return (Left (ExprErr "Function call" "Number of argument does not match or there is no return statement"))

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: InputM ()
repl = do st <- lift get
          inp <- case commands st of
            [] -> getInputLine "> "
            (x:xs) -> do lift $ put st {commands = xs}
                         return (Just x)
          st <- lift get
          case inp of
            Nothing    -> return ()
            Just input ->
                case parse pStatement input of
                    [(cmd, "")] -> -- Must parse entire input
                      case cmd of
                        (Set var e) -> do st' <- process st cmd
                                          if var `notElem` wordList st'
                                            then do lift $ put st' {wordList = var : wordList st'}
                                                    repl
                                          else do lift $ put st' {wordList = wordList st'}
                                                  repl
                        Quit -> return()
                        (Func name _ _)    -> do st' <- process st cmd
                                                 if (name ++ "(") `notElem` wordList st
                                                  then do lift $ put st' {wordList = (name ++ "(") : wordList st'}
                                                          repl
                                                 else do lift $ put st' {wordList = wordList st'}
                                                         repl
                        (Expr expr) -> do process st (Print expr)
                                          repl
                        _ -> do st' <- process st cmd
                                lift $ put st'
                                repl
                    _ -> do outputStrLn "Parse error"
                            repl