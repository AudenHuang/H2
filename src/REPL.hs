{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module REPL where

import System.Console.Haskeline
import Expr
import Parsing
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict (get, put, runStateT, StateT)

type StateM = StateT State IO
type InputM = InputT StateM
data State = State {vars :: BTree, commands :: [String],
                    functions :: [(Name, [Name], [Command])],
                    wordList :: [String]}

initHLCompletionList :: [String]
initHLCompletionList = ["print", "def", "while", "if", "else", "toFloat(", "toInt(", "toString(", "quit", "True", "False"]

initState :: State
initState = State Leaf [] [("", [""], [])] initHLCompletionList

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> BTree -> BTree
updateVars _name _value Leaf = Node (_name, _value) Leaf Leaf
updateVars _name _value (Node (name, value) ltree rtree)
  | _name < name = Node (name, value) (updateVars _name _value ltree) rtree
  | _name > name = Node (name, value) ltree (updateVars _name _value rtree)
  | otherwise = Node (name, _value) ltree rtree

-- Return a new set of variables with the given name removed
dropVar :: Name -> BTree -> BTree
dropVar _name Leaf = Leaf
dropVar _name (Node (name, value) ltree rtree)
  | _name < name = Node (name, value) (dropVar _name ltree) rtree
  | _name > name = Node (name, value) ltree (dropVar _name rtree)
  | otherwise = case (ltree, rtree) of
    (Leaf, _) -> rtree
    (_, Leaf) -> ltree
    _ -> let (left_largest_var, left_largest_val) = largestVar ltree
             largestVar tree = case tree of -- using "last (inorderTraversal tree)" or something like that is against the purpose of using binary search tree.
                                    Node (name_, value_) _ Leaf -> (name_, value_)
                                    Node (_, _) _ rtree -> largestVar rtree
             in Node (left_largest_var, left_largest_val) (dropVar left_largest_var ltree) rtree

updateFunctions :: Name -> [Name] -> [Command] -> [(Name, [Name], [Command])] -> [(Name, [Name], [Command])]
updateFunctions name _vars _commands [] = [(name, _vars, _commands)]
updateFunctions name _vars _commands ((n, v, c): funs)
  | name == n = (name, _vars, _commands) : funs
  | otherwise = (n, v, c) : updateFunctions name _vars _commands funs

process :: State -> Command -> InputT StateM State
process st (Set var e) =
  do
    case eval (vars st) e of
      Left (ExprErr expr err_msg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ err_msg)
                                        return st -- error
      Right Input -> do inpVal <- getInputLine "Input > "
                        case inpVal of
                         Just inp -> return (st {vars = updateVars var (StrVal inp) (vars st)})
                         Nothing -> return (st {vars = updateVars var (StrVal "") (vars st)})
      Right (FunCall name exprs) -> do val <- funCallVal st name exprs
                                       case val of
                                        Right eval_res -> return st {vars = updateVars var eval_res (vars st)}
                                        Left (ExprErr expr err_msg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ err_msg)
                                                                          return st -- error
      Right eval_res -> do
        let st' = st {vars = updateVars var eval_res (vars st)}
        -- st' should include the variable set to the result of evaluating e
        return st'
process st (Print e) =
  do
    case eval (vars st) e of
         Left (ExprErr expr err_msg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ err_msg)
         Right Input -> do inpVal <- getInputLine "Input > "
                           case inpVal of
                                Just inp -> do outputStrLn inp
                                Nothing -> do outputStrLn ""
         Right (FunCall name exprs) -> do val <- funCallVal st name exprs
                                          case val of
                                            Right eval_res -> outputStrLn (show eval_res) -- how will it show a string
                                            Left (ExprErr expr err_msg) -> do outputStrLn ("Error on " ++ expr ++ ": " ++ err_msg)
         Right eval_res -> do
           outputStrLn (show eval_res)
    return st

process st (IfE e b1 b2) = case eval (vars st) e of
  Right (BoolVal True)  -> do processBlock st b1
  Right (BoolVal False) -> do processBlock st b2
  _                    -> do outputStrLn "Invalid if conditional"
                             return st
process st (If e b1) = case eval (vars st) e of
  Right (BoolVal True)  -> do processBlock st b1
  Right (BoolVal False) -> do return st
  _                     -> do outputStrLn "Invalid if conditional"
                              return st

process st (While e block) = loop st block e
  where loop :: State -> [Command] -> Expr -> InputT StateM State
        loop state cmds expr = case eval (vars state) expr of
          Right (BoolVal True)  -> do st' <- processBlock state cmds
                                      loop st' cmds expr
          Right (BoolVal False) -> return state
          _                    -> do outputStrLn "Invalid while conditional"
                                     return state
process st (Func name vars commands) = return st {functions = updateFunctions name vars commands (functions st)}
process st (VoidFuncCall name exprs) = case func of
  [] -> return st
  [(fname, vnames, commands)] -> if length exprs == length vnames && blockIsVoid commands
                                    then do sState <- assignVals scopedState vnames exprs
                                            processBlock sState commands
                                            return st
                                 else return st
  where scopedState :: State
        scopedState = st
        func :: [(Name, [Name], [Command])]
        func = filter (\(x, _, _) -> x == name) (functions st)
        assignVals :: State -> [Name] -> [Expr] -> InputT StateM State
        assignVals state (v:vs) (e:es) = do state' <- process state (Set v e)
                                            assignVals state' vs es
        assignVals state []     []     = return state

processBlock :: State -> [Command] -> InputT StateM State
processBlock st [cmd]   = do process st cmd
processBlock st (cmd: cmds) = do st' <- process st cmd
                                 processBlock st' cmds
processBlock st _           = return st

processBlockRet :: (State, Either Error Expr) -> [Command] -> InputT StateM (State, Either Error Expr)
processBlockRet (st, _) (Return e: _)   = return (st, Right e)
processBlockRet (st, _) [cmd]   = do st' <- process st cmd
                                     return (st', Left (ExprErr "Function call" "No return statement"))
processBlockRet (st, _) (cmd: cmds) = do st' <- process st cmd
                                         processBlockRet (st', Left (ExprErr "" "")) cmds
-- processBlockRet (st, _) _           = return st

blockIsVoid :: [Command] -> Bool
blockIsVoid []            = True
blockIsVoid (Return x: _) = False
blockIsVoid (x: xs)       = blockIsVoid xs

-- TODO: convert to either | Update Errors
funCallVal :: State -> Name -> [Expr] -> InputT StateM (Either Error Value)
funCallVal st name exprs = case fun of
        [] -> return (Left (ExprErr "Function call" "No such function"))
        [(fname, vnames, commands)] -> if length exprs == length vnames && not(blockIsVoid commands)
                                          then do sState <- assignVals scopedState vnames exprs
                                                  (st', e) <- processBlockRet (sState, Left (ExprErr "" "")) commands
                                                  case e of
                                                    Right expression -> return (eval (vars st') expression)
                                                    Left (ExprErr expr err_msg) -> return (Left (ExprErr expr err_msg)) -- Should never happen
                                       else return (Left (ExprErr "Function call" "Number of argument does not match or there is no return statement"))
        where scopedState :: State
              scopedState = st
              fun :: [(Name, [Name], [Command])]
              fun = filter (\(x, _, _) -> x == name) (functions st)
              assignVals :: State -> [Name] -> [Expr] -> InputT StateM State
              assignVals state (v:vs) (e:es) = do state' <- process state (Set v e)
                                                  assignVals state' vs es
              assignVals state []     []     = return state

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
                                          lift $ put st' {wordList = var : wordList st'}
                                          repl
                        Quit -> return()
                        (Func name _ _)    -> do st' <- process st cmd
                                                 lift $ put st' {wordList = (name ++ "(") : wordList st'}
                                                 repl
                        (Expr expr) -> do process st (Print expr)
                                          repl
                        _ -> do st' <- process st cmd
                                lift $ put st'
                                repl
                    _ -> do outputStrLn "Parse error"
                            repl
