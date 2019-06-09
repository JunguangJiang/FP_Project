-- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
module EvalType where

import AST
import Control.Monad.State
import Text.Printf
import Data.Map.Strict as M

type TEnv = Map String Type

data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
    getVars :: M.Map String Type,
    getCtors :: M.Map String ([Type], String)                        
} deriving (Show, Eq)

type ContextState a = StateT Context Maybe a

withVars :: Map String Type -> ContextState a -> ContextState a
withVars = undefined

withVar :: String -> Type -> ContextState a -> ContextState a
withVar x t op = do
    env <- get --save current state
    modify $ \s -> env { getVars = M.insert x t (getVars s)}
    r <- op
    put env -- recover state
    return r

findVar :: String -> ContextState Type
findVar = undefined

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

areTwoInts :: Expr -> Expr -> ContextState Type
areTwoInts e1 e2 = do
    isInt e1
    isInt e2

isSameType :: Expr -> Expr -> ContextState Type
isSameType = undefined

evalLetRec :: String -> String -> Type -> Expr -> Type -> Expr -> ContextState Type
evalLetRec = undefined

evalApply :: Expr -> Expr -> ContextState Type
evalApply = undefined

getADTTypes :: [ADT] -> Map String Type
getADTTypes = undefined

getADTCtors :: [ADT] -> Map String ([Type], String)
getADTCtors = undefined

matchPT :: Pattern -> Type -> Map String ([Type], String) -> Maybe (Map String Type)
matchPT = undefined

evalOneCase :: Type -> (Pattern, Expr) -> ContextState Type
evalOneCase = undefined

evalCase :: Expr -> [(Pattern, Expr)] -> ContextState Type
evalCase = undefined

evalEq :: Expr -> Expr -> ContextState Type
evalEq = undefined

evalOrd :: Expr -> Expr -> ContextState Type
evalOrd = undefined

eval :: Expr -> ContextState Type
eval (EBoolLit _) = return TBool
eval (ENot e) = do 
    isBool e 
    return TBool
eval (EIntLit _) = return TInt
eval (EVar x) = do
    env <- get
    case getVars env M.!? x of
        Just t -> return t
        Nothing -> lift Nothing  
eval (EAdd e1 e2) = areTwoInts e1 e2
eval (ESub e1 e2) = areTwoInts e1 e2
eval (EMul e1 e2) = areTwoInts e1 e2
eval (EDiv e1 e2) = areTwoInts e1 e2
eval (ELambda (x,t) e1) = do
    t1 <- withVar x t $ eval e1
    return $ TArrow t t1
eval _ = undefined


--     EApply e1 e2 -> do
--         t1 <- infer e1
--         t2 <- infer e2
--         case t1 of
--             TArrow t11 t12 -> if t11 == t2
--                 then return t12
--                 else err $ printf "Argument expression %s has wrong type\n" e2
--             _ -> err $ printf "Arrow type expected: %s\n" e1
--     ELet (x, e1) e2 -> do
--         t1 <- infer e1
--         withVar x t1 $ infer e2

-- typeCheck :: Expr -> TEnv -> Either String Type
-- typeCheck e = evalStateT $ infer e 

evalType :: Program -> Maybe Type
evalType (Program adts body) = evalStateT (eval body) $ Context{getVars = M.empty, getCtors = M.empty}