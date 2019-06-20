-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State
import Text.Printf
import Data.Map.Strict as M
import Control.Monad

-- type TEnv = Map String Type

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
    getVars :: M.Map String Type,
    getCtors :: M.Map String ([Type], String)                        
} deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

withVars :: Map String Type -> ContextState a -> ContextState a
withVars map op = do
    env <- get
    modify $ \s -> env {getVars = M.union map (getVars s)}
    r <- op
    put env
    return r

withVar :: String -> Type -> ContextState a -> ContextState a
withVar x t op = do
    env <- get --save current state
    modify $ \s -> env { getVars = M.insert x t (getVars s)}
    r <- op
    put env -- recover state
    return r

findVar :: String -> ContextState Type
findVar x = do
    env <- get 
    case getVars env M.!? x of
        Just t -> return t
        Nothing -> lift Nothing 

findCtors :: String -> ContextState ([Type], String)
findCtors = undefined

isBool :: Expr -> ContextState Type
isBool e = do
  et <- eval e
  case et of
    TBool -> return TBool
    _ -> lift Nothing

isInt :: Expr -> ContextState Type
isInt e = do
    t <- eval e
    if t == TInt
        then return TInt
        else lift Nothing

isChar :: Expr -> ContextState Type
isChar e = do
    t <- eval e
    if t == TChar
        then return TChar
        else lift Nothing

areTwoInts :: Expr -> Expr -> ContextState Type
areTwoInts e1 e2 = do
    isInt e1
    isInt e2

isType :: Expr -> [Type] -> ContextState Type
isType e types = do
    t <- eval e
    if t `elem` types
        then return t
        else lift Nothing

isSameType :: Expr -> Expr -> ContextState Type
isSameType e1 e2 = do
    t1 <- eval e1
    t2 <- eval e2
    if t1 == t2 then return TBool
    else lift Nothing

evalLetRec :: String -> String -> Type -> Expr -> Type -> Expr -> ContextState Type
evalLetRec = undefined

evalApply :: Expr -> Expr -> ContextState Type
evalApply = undefined

getADTTypes :: [ADT] -> Map String Type
getADTTypes = undefined

getADTCtor :: ADT -> Map String ([Type], String)
getADTCtor (ADT adtName conss) = Prelude.foldl (\upd (cons, types) -> M.insert cons (types, adtName) upd) M.empty conss

getADTCtors :: [ADT] -> Map String ([Type], String)
getADTCtors = Prelude.foldl (\upd adt -> M.union upd (getADTCtor adt)) M.empty

matchPT :: Pattern -> Type -> M.Map String ([Type], String) -> Maybe (M.Map String Type)
matchPT (PBoolLit _) t _ = if t == TBool then return M.empty else Nothing
matchPT (PIntLit _)  t _ = if t == TInt then return M.empty else Nothing
matchPT (PCharLit _) t _ = if t == TChar then return M.empty else Nothing
matchPT (PVar s) t _ = return $ M.fromList [(s, t)]
matchPT (PData con patterns) t ctor = do
    (types, adtName) <- ctor M.!? con
    let (TData adtName') = t
    if adtName == adtName' && length patterns == length types then foldM 
        (\upd (p', t') -> do
            map <- matchPT p' t' ctor
            return $ M.union upd map
        ) M.empty (zip patterns types)
    else Nothing

evalOneCase :: Type -> (Pattern, Expr) -> ContextState Type
evalOneCase t (p, e) = do
    env <- get 
    case matchPT p t (getCtors env) of
        Just vars -> withVars vars $ eval e
        _ -> lift Nothing

evalCase :: Expr -> [(Pattern, Expr)] -> ContextState Type
evalCase e pes = do
    t <- eval e
    types <- foldM (\upd pe -> do
            t' <- evalOneCase t pe
            return (t' : upd)
        ) [] pes
    let first_type = head types
    if all (==first_type) types then return first_type else lift Nothing

evalEq :: Expr -> Expr -> ContextState Type
evalEq e1 e2 = do
    isType e1 [TBool, TInt, TChar]
    isSameType e1 e2

evalOrd :: Expr -> Expr -> ContextState Type
evalOrd e1 e2 = do
    isType e1 [TInt, TChar]
    isSameType e1 e2

eval :: Expr -> ContextState Type
eval (EBoolLit _) = return TBool
eval (EIntLit _) = return TInt
eval (ECharLit _) = return TChar
eval (ENot e) = isBool e 

eval (EAnd e1 e2) = do
    isBool e1
    isBool e2
eval (EOr e1 e2) = do
    isBool e1
    isBool e2
eval (EAdd e1 e2) = areTwoInts e1 e2
eval (ESub e1 e2) = areTwoInts e1 e2
eval (EMul e1 e2) = areTwoInts e1 e2
eval (EDiv e1 e2) = areTwoInts e1 e2
eval (EMod e1 e2) = areTwoInts e1 e2
eval (EEq e1 e2) = evalEq e1 e2
eval (ENeq e1 e2) = evalEq e1 e2
eval (ELt e1 e2) = evalOrd e1 e2
eval (EGt e1 e2) = evalOrd e1 e2
eval (ELe e1 e2) = evalOrd e1 e2
eval (EGe e1 e2) = evalOrd e1 e2

eval (EIf e1 e2 e3) = do
    t1 <- eval e1
    t2 <- eval e2
    t3 <- eval e3
    if t1 == TBool && t2 == t3 then return t2
    else lift Nothing

eval (ELambda (pn,pt) e) = do
    t <- withVar pn pt $ eval e
    return $ TArrow pt t
eval (ELet (n, e1) e2) = do
    t1 <- eval e1
    withVar n t1 $ eval e2
eval (ELetRec f (x, tx) (e1, ty) e2) = do
    ty_ <- withVar f (TArrow tx ty) . withVar x tx $ eval e1
    if ty_ /= ty then lift Nothing
    else withVar f (TArrow tx ty) $ eval e2

eval (EVar x) = do
    env <- get 
    case getVars env M.!? x of
        Just t -> return t
        Nothing -> case getCtors env M.!? x of
            (Just (types, adtName)) -> return $ Prelude.foldr TArrow (TData adtName) types
            Nothing -> lift Nothing

eval (EApply e1 e2) = do
    t1 <- eval e1
    t2 <- eval e2
    case t1 of
        TArrow t11 t12 -> if t11 == t2 then return t12 else lift Nothing
        _ -> lift Nothing

eval (ECase e pes) = evalCase e pes

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $ Context{getVars = M.empty, getCtors = getADTCtors adts}