
--------------------------------------------------------------------------------
-- Functional Programming (CS256)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

module Interpreter where
--------------------------------------------------------------------------------

import Language
import Data.Typeable

--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory = [(String, Int)]

-- | Enumerates reasons for errors.
data Err
   = DivByZeroError                    -- ^ Division by zero was attempted.
   | NegativeExponentError             -- ^ Raising a number to a negative
                                       -- exponent was attempted.
   | UninitialisedMemory String        -- ^ Tried to read from a variable
                                       -- that does not exist.
   deriving (Eq, Show)

--------------------------------------------------------------------------------

operate :: Op -> Int -> Int -> Either Err Int
operate Add a b = Right (a + b)
operate Sub a b = Right (a - b)
operate Mul a b = Right (a * b)
operate Div a b = case safeDiv a b of
            Left x -> Left x
            Right x -> Right x
operate Pow a b | b < 0 = Left NegativeExponentError
                | otherwise = Right (a ^ b)
operate Equal a b          | a == b = Right 1
                           | otherwise = Right 0
operate Neq a b            | a /= b = Right 1
                           | otherwise = Right 0
operate LessThan a b       | a < b = Right 1
                           | otherwise = Right 0
operate LessOrEqual a b    | a <= b = Right 1
                           | otherwise = Right 0
operate GreaterThan a b    | a > b = Right 1
                           | otherwise = Right 0
operate GreaterOrEqual a b | a >= b = Right 1
                           | otherwise = Right 0

safeDiv :: Int -> Int -> Either Err Int
safeDiv x 0 = Left DivByZeroError
safeDiv x y = Right (x `div` y)

bind :: Either Err a -> (a -> Either Err b) -> Either Err b
bind (Left x)  f = Left x
bind (Right x) f = f x

evalExpr :: Expr -> Memory -> Either Err Int
evalExpr (ValE val) mem = Right val
evalExpr (VarE name) mem = lookUp name mem
evalExpr (BinOpE op expr1 expr2) mem = evalExpr expr1 mem `bind` \x ->
                                       evalExpr expr2 mem`bind` \y ->
                                       operate op x y

    -- case evalExpr expr1 mem of
    --                     Left x -> Left x
    --                     Right x -> case evalExpr expr2 mem of
    --                         Left y -> Left y
    --                         Right y -> operate op x y

evalIf :: [(Expr, [Stmt])] -> Memory-> Either Err [Stmt]
evalIf [] mem = Right []
evalIf ((expr, ys):xs) mem = evalExpr expr mem `bind` \x ->
                             if x /= 0 then Right ys else evalIf xs mem

    -- case evalExpr expr mem of
    --                     Left x -> Left x
    --                     Right x | x /= 0 -> Right ys
    --                             | otherwise -> evalIf xs mem


lookUp :: String -> Memory -> Either Err Int
lookUp name [] = Left (UninitialisedMemory "x")
lookUp name (x:xs) | fst x == name = Right (snd x)
                   | otherwise = lookUp name xs

updateOrAdd :: String -> Int -> Memory -> Bool -> Memory
updateOrAdd name val [] found       | found = []
                                    | otherwise = [(name,val)]
updateOrAdd name val (x:xs) found   | fst x == name = (name,val) : updateOrAdd name val xs True
                                    | otherwise = x : updateOrAdd name val xs found

repeatStmts :: Int -> [Stmt] -> [Stmt]
repeatStmts 0 stmts  = []
repeatStmts num stmts = stmts ++ repeatStmts (num - 1) stmts

-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
interpret :: Program -> Memory -> Either Err Memory
interpret [] mem = Right mem
interpret (AssignStmt name (ValE val):xs) mem = interpret xs (updateOrAdd name val mem False)
interpret (AssignStmt name (VarE var):xs) mem = evalExpr (VarE var) mem `bind` \x -> interpret xs (updateOrAdd name x mem False)
interpret (AssignStmt name (BinOpE op expr1 expr2):xs) mem = evalExpr (BinOpE op expr1 expr2) mem `bind` \x -> interpret xs (updateOrAdd name x mem False)
interpret (IfStmt ifexpr ifstmts ifelses elses:xs) mem = evalExpr ifexpr mem `bind` \x ->
        if x /= 0 then interpret (ifstmts ++ xs) mem else evalIf ifelses mem `bind` \x ->
                if not (null x) then interpret (x ++ xs) mem else interpret (elses ++ xs) mem
interpret (RepeatStmt expr stmts:xs) mem = evalExpr expr mem `bind` \x ->
                                            interpret (repeatStmts x stmts ++ xs) mem



expand :: [Stmt] -> Memory -> Either Err [Stmt]
expand [] mem = Right []
expand (RepeatStmt expr stuff:xs) mem = evalExpr expr mem `bind` \x ->
                                        expand xs mem `bind` \y -> (repeatStmts x stuff) ++ expand xs mem
expand (x:xs) mem = x : expand xs mem
-----------------------------------------------------------------
