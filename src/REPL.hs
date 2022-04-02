module REPL where

import System.Console.Haskeline
import Expr
import Parsing
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict (get, put, runStateT, StateT)

-- Reference:  https://www.reddit.com/r/haskell/comments/1os0yq/haskeline_woes/
type StateM = StateT LState IO
type InputM = InputT StateM

-- the state keeping track of the variables, commands, functions and word list
data LState = LState {vars :: BinTree, cmds :: [String],
                    functions :: [(Name, [Name], [Command])],
                    wrds :: [String]}

-- initialisation for haskeline tab completion list
initHLList :: [String]
initHLList = ["print", "def", "while", "if", "else", "toFloat(", "toInt(", "toString(", "quit", "True", "False"]

-- initialisation for the state at the start of the program
initLState :: LState
initLState = LState Leaf [] [("", [""], [])] initHLList

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
-- The variables are stored in BinTree in some sense of alphabetic order
updateVars :: Name -> Value -> BinTree -> BinTree
updateVars name' value' Leaf = Node (name', value') Leaf Leaf
updateVars name' value' (Node (name, value) binTreeL binTreeR)
  | name' > name = Node (name, value) binTreeL (updateVars name' value' binTreeR)
  | name' < name = Node (name, value) (updateVars name' value' binTreeL) binTreeR
  | otherwise = Node (name, value') binTreeL binTreeR

-- Given a function name, variables and block of commands
-- the function will be returned in form of [(function_name, [variables], [commands])]
updateFunctions :: Name -> [Name] -> [Command] -> [(Name, [Name], [Command])] -> [(Name, [Name], [Command])]
-- if funcitons in LState is empty, the function will be returned in the formatted way
updateFunctions name' vars' cmds' [] = [(name', vars', cmds')]
updateFunctions name' vars' cmds' ((name, vars, cmds): funcs)
-- if the function name exists in functions in LState, we will update it
  | name' == name = (name', vars', cmds') : funcs
--  otherwise we will move from left to right. Each recursion with one element less then the previous. (like (x:xs))
  | otherwise = (name, vars, cmds) : updateFunctions name' vars' cmds' funcs

-- Given the state, function name and expressions, it will return an error or a value of the specified function
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

-- Given state and block of commands, the commands will be executed
processBlock :: LState -> [Command] -> InputT StateM LState
-- Execute the only one command
processBlock st [cmd]   = do process st cmd
-- execute the first command and execute the rest one by one
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

-- process the input command and return the updated state
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

-- process the print command
process st (Print expr) =
  do
    case eval (vars st) expr of
         Left (ErrorExpr expr errorMsg) -> do outputStrLn ("Error : " ++ expr ++ ": " ++ errorMsg)
         Right (FunCall name exprs) -> do val <- funCallVal st name exprs
                                          case val of
                                            Left (ErrorExpr expr errorMsg) -> do outputStrLn ("Error : " ++ expr ++ ": " ++ errorMsg)
                                            -- print the value of the evaluated results
                                            Right eval_res -> outputStrLn (show eval_res) 
         Right Input -> do inpVal <- getInputLine "Input > "
                           case inpVal of
                             -- print the value of the input
                                Just input -> do outputStrLn input
                                Nothing -> do outputStrLn ""
        -- print the value of the evaluated results
         Right eval_res -> do outputStrLn (show eval_res)
    return st

-- process if else statement
process st (IfE expr t f) = case eval (vars st) expr of
  Right (BoolVal True)  -> do processBlock st t
  Right (BoolVal False) -> do processBlock st f
  -- if the conditional statement is NOT valid since it doesn't return a boolean value
  _                     -> do outputStrLn "Invalid if conditional"
                              return st

-- process if (without else) statement
process st (If expr t) = case eval (vars st) expr of
  Right (BoolVal True)  -> do processBlock st t
  Right (BoolVal False) -> do return st
  _                     -> do outputStrLn "Invalid if conditional"
                              return st

-- process the while loop
process st (While expr block) = let loop :: LState -> [Command] -> Expr -> InputT StateM LState
                                    loop lstate cmds expr = case eval (vars lstate) expr of
                                                                 Right (BoolVal True)  -> do st' <- processBlock lstate cmds
                                                                                             loop st' cmds expr
                                                                --  if the conditional statement is NOT valid since it doesn't return a boolean value
                                                                 Right (BoolVal False) -> return lstate
                                                                 _                    -> do outputStrLn "Invalid while conditional"
                                                                                            return lstate
                                    in loop st block expr

-- update functions of the LState
process st (Func name vars cmds) = return st {functions = updateFunctions name vars cmds (functions st)}

-- process the specified function
process st (FuncCall name exprs) = let scope :: LState
                                       scope = st
                                       func :: [(Name, [Name], [Command])]
                                       func = filter (\(a, _, _) -> a == name) (functions st)
                                       --  set variables for the function scope one by one
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
-- get the current state of the program
repl = do st <- lift get
-- check the commands in the state
          input <- case cmds st of
            [] -> getInputLine "> "
            (x:xs) -> do lift $ put st {cmds = xs}
                         return (Just x)
          st <- lift get
          case input of
            Nothing    -> return ()
            -- parse the user input
            Just input ->
                case parse pCommand input of
                    [(cmd, "")] -> 
                      -- check the type of command and process accordingly
                      case cmd of
                        (Import filepath) -> do text <- lift $ lift (readFile filepath)
                                                lift $ put st {cmds = lines text ++ cmds st}
                                                repl
                        (Set var expr) -> do st' <- process st cmd
                                             if var `notElem` wrds st'
                                              then do lift $ put st' {wrds = var : wrds st'}
                                                      repl
                                             else do lift $ put st' {wrds = wrds st'}
                                                     repl
                        -- quit the main function, still in >Main.hs
                        Quit -> return()
                        -- process the specified function name
                        (Func name _ _)    -> do st' <- process st cmd
                                                 if (name ++ "(") `notElem` wrds st
                                                    then do lift $ put st' {wrds = (name ++ "(") : wrds st'}
                                                            repl
                                                 else do lift $ put st' {wrds = wrds st'}
                                                         repl
                        -- print the expression
                        (Expr expr) -> do process st (Print expr)
                                          repl
                        _ -> do st' <- process st cmd
                                lift $ put st'
                                repl
                    _ -> do outputStrLn "Parse error"
                            repl