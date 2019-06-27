-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalValue :: Program -> Result 就行。
module EvalValue where

import AST
import Control.Monad.State
import Data.Map.Strict as M
import Control.Monad.Loops

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VClosure Context String Expr
  | VAdt String [Value]
  | VAdtFun String [Value] Int
  deriving (Show, Eq)

newtype Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
  getVars :: M.Map String Value
} deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

getBool :: Expr -> ContextState Bool
getBool e = do
  ev <- eval e
  case ev of
    VBool b -> return b
    _ -> lift Nothing
  
getInt :: Expr -> ContextState Int
getInt e = do
  ev <- eval e
  case ev of
    VInt i -> return i
    _ -> lift Nothing

getInt2 :: Expr -> Expr -> ContextState (Int, Int)
getInt2 e1 e2 = do
  v1 <- getInt e1
  v2 <- getInt e2
  return (v1, v2)

getChar' :: Expr -> ContextState Char
getChar' e = do
  ev <- eval e
  case ev of
    VChar c -> return c
    _ -> lift Nothing

evalEq :: Expr -> Expr -> ContextState Bool
evalEq e1 e2 = do
  v1 <- eval e1
  case v1 of
    VBool b1 -> do
      b2 <- getBool e2
      return $ b1 == b2
    VInt i1 -> do
      i2 <- getInt e2
      return $ i1 == i2
    VChar c1 -> do
      c2 <- getChar' e2
      return $ c1 == c2
    _ -> lift Nothing

evalOrd :: (Int -> Int -> Bool) -> (Char -> Char -> Bool) -> Expr -> Expr -> ContextState Bool
evalOrd cmpi cmpc e1 e2 = do
  v1 <- eval e1
  case v1 of
    VInt i1 -> do
      i2 <- getInt e2
      return $ cmpi i1 i2
    VChar c1 -> do
      c2 <- getChar' e2
      return $ cmpc c1 c2
    _ -> lift Nothing

withVar :: String -> Value -> ContextState a -> ContextState a
withVar x v a = do
  env <- get
  modify $ \s -> env { getVars = M.insert x v (getVars s)}
  r <- a
  put env
  return r

withVars :: M.Map String Value -> ContextState a -> ContextState a
withVars vars a = do
  env <- get
  modify $ \s -> env {getVars = M.union vars (getVars s)}
  r <- a
  put env
  return r

getADTCtor :: ADT -> Map String Value
getADTCtor (ADT adtName conss) = Prelude.foldl (\upd (cons, types) -> M.insert cons 
  (if Prelude.null types then VAdt cons [] else VAdtFun cons [] (length types)) 
  upd ) M.empty conss

getADTCtors :: [ADT] -> Map String Value
getADTCtors = Prelude.foldl (\upd adt -> M.union (getADTCtor adt) upd) M.empty

matchPV :: Pattern -> Value -> Maybe (Map String Value)
matchPV (PBoolLit p) (VBool v) = if p == v then return M.empty else Nothing
matchPV (PIntLit p) (VInt v) = if p == v then return M.empty else Nothing
matchPV (PCharLit p) (VChar v) = if p == v then return M.empty else Nothing
matchPV (PVar s) v = return $ M.fromList [(s, v)]
matchPV (PData con patterns) (VAdt cons values) = 
  if con == cons then 
    foldM (\upd (p, v) -> do
    map <- matchPV p v
    return $ M.union map upd
    ) M.empty (zip patterns values)
  else Nothing
matchPV _ _ = Nothing

evalCase :: Expr -> [(Pattern, Expr)] -> ContextState Value
evalCase e pes = do
  v <- eval e
  matched <- return $ firstM (\(p, expr) -> case matchPV p v of 
    Just _ -> Just True
    Nothing -> Just False) pes
  case matched of
    Just (Just (p, expr)) -> let (Just vars) = matchPV p v in withVars vars $ eval expr
    _ -> lift Nothing

eval :: Expr -> ContextState Value
eval (EBoolLit b) = return $ VBool b
eval (EIntLit i) = return $ VInt i
eval (ECharLit c) = return $ VChar c
eval (ENot e) = do
  b <- getBool e
  return (VBool $ not b)
eval (EAnd e1 e2) = do
  v1 <- getBool e1
  if not v1 then return (VBool False) else eval e2
eval (EOr e1 e2) = do
  v1 <- getBool e1
  if v1 then return (VBool True) else eval e2
eval (EAdd e1 e2) = do
  (v1, v2) <- getInt2 e1 e2
  return $ VInt (v1 + v2)
eval (ESub e1 e2) = do
  (v1, v2) <- getInt2 e1 e2
  return $ VInt (v1 - v2)
eval (EMul e1 e2) = do
  (v1, v2) <- getInt2 e1 e2
  return $ VInt (v1 * v2)
eval (EDiv e1 e2) = do
  (v1, v2) <- getInt2 e1 e2
  if v2 == 0 then lift Nothing else return $ VInt (v1 `div` v2)
eval (EMod e1 e2) = do
  (v1, v2) <- getInt2 e1 e2
  if v2 == 0 then lift Nothing else return $ VInt (v1 `mod` v2)
eval (EEq e1 e2) = do
  b <- evalEq e1 e2
  return $ VBool b
eval (ENeq e1 e2) = do
  b <- evalEq e1 e2
  return $ VBool (not b)
eval (ELt e1 e2) = do
  b <- evalOrd (<) (<) e1 e2
  return $ VBool b
eval (EGt e1 e2) = do
  b <- evalOrd (>) (>) e1 e2
  return $ VBool b
eval (ELe e1 e2) = do
  b <- evalOrd (<=) (<=) e1 e2
  return $ VBool b
eval (EGe e1 e2) = do
  b <- evalOrd (>=) (>=) e1 e2
  return $ VBool b

eval (EIf e1 e2 e3) = do
  v1 <- eval e1
  case v1 of
    VBool True -> eval e2
    VBool False -> eval e3
    _ -> lift Nothing

eval (ELambda (pn, pt) e) = do
  env <- get
  -- let env' = env {getVars = M.filterWithKey (\x _ -> x /= pn) (getVars env)}
  return $ VClosure env pn e

eval (ELet (n, e1) e2) = do
  v1 <- eval e1
  withVar n v1 $ eval e2

eval (ELetRec f (x, tx) (e1, ty) e2) = do
  env <- get
  let fv = VClosure env x e1
  withVar f fv $ eval e2

eval (EVar x) = do
  env <- get
  case getVars env M.!? x of
    Just v -> return v
    Nothing -> lift Nothing  

eval (EApply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case v1 of
    (VClosure env x b) ->
      withVars (getVars env) . withVar x v2 $ eval b
    (VAdtFun cons values left) -> 
      if left == 1 then return (VAdt cons new_values) else return (VAdtFun cons new_values (left-1)) where new_values = values++[v2]

eval (ECase e pes) = evalCase e pes

evalProgram :: Program -> Maybe Value
evalProgram (Program adts body) = evalStateT (eval body) $
  Context {getVars = getADTCtors adts } -- 可以用某种方式定义上下文，用于记录变量绑定状态


evalValue :: Program -> Result
evalValue p = case evalProgram p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid