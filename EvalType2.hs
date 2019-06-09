-- -- | 这是其中一种实现方式的代码框架。你可以参考它，或用你自己的方式实现，只要按需求完成 evalType :: Program -> Maybe Type 就行。
-- module EvalType where

-- import AST
-- import Control.Monad.State
-- import Text.Printf
-- import Data.Map.Strict as M

-- type TEnv = M.Map String Type

-- -- data Context = Context { -- 可以用某种方式定义上下文，用于记录变量绑定状态
-- --                        }
-- --   deriving (Show, Eq)

-- -- type ContextState a = StateT Context Maybe a

-- -- isBool :: Expr -> ContextState Type
-- -- isBool e = do
-- --   et <- eval e
-- --   case et of
-- --     TBool -> return TBool
-- --     _ -> lift Nothing

-- -- eval :: Expr -> ContextState Type
-- -- eval (EBoolLit _) = return TBool
-- -- eval (ENot e) = isBool e >> return TBool
-- -- -- ... more
-- -- eval _ = undefined



-- type Ctx = StateT TEnv (Either String)

-- err :: String -> Ctx a
-- err s = lift $ Left s

-- expectInt :: Expr -> Ctx Type
-- expectInt e = do
--     t <- infer e
--     if t == TInt
--         then return TInt
--         else err $ printf "Int type expected"

-- infer :: Expr -> Ctx Type
-- infer e = case e of
--     EIntLit _ -> return TInt
--     EVar x -> do
--         env <- get
--         case env M.!? x of
--             Just t -> return t
--             Nothing -> err $ printf "Variable %s not found\n" x
--     EAdd e1 e2 -> do
--         expectInt e1
--         expectInt e2
--     ESub e1 e2 -> do
--         expectInt e1
--         expectInt e2
--     EMul e1 e2 -> do
--         expectInt e1
--         expectInt e2
--     EDiv e1 e2 -> do
--         expectInt e1
--         expectInt e2
--     ELambda (x, t) e1 -> do
--         t1 <- withVar x t $ infer e1
--         return $ TArrow t t1
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

-- withVar :: String -> Type -> Ctx a -> Ctx a
-- withVar x t op = do
--     env <- get --save current state
--     modify $ M.insert x t
--     r <- op
--     put env -- recover state
--     return r

-- typeCheck :: Expr -> TEnv -> Either String Type
-- typeCheck e = evalStateT $ infer e 

-- evalType :: Program -> Maybe Type
-- -- evalType (Program adts body) = evalStateT (eval body) $
-- --   Context {  } -- 可以用某种方式定义上下文，用于记录变量绑定状态
-- evalType (Program adts body) = 
--     case typeCheck body M.empty of
--         Left err -> Nothing
--         Right t -> Just t


