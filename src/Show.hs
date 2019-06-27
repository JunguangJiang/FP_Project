module Show where

import AST
import EvalType
import EvalValue

-- | Shortcut to define function with @ELetRec@
makeFun :: (String, Type) -> [(String, Type)] -> Expr -> (Expr -> Expr)
makeFun (fn, rt) ((p, t):pts) body =
  let helper [] = body
      helper ((p0, t0):rs) = ELambda (p0, t0) (helper rs)
      ts = map snd pts ++ [rt]
  in ELetRec fn (p, t) (helper pts, foldr1 TArrow ts)

callFun :: Expr -> [Expr] -> Expr
callFun f [e] = EApply f e
callFun f (e:es) = callFun (EApply f e) es

-- Maybe Int 和 [Int] 
maybeInt = ADT "Maybe" [("Just", [TInt]), ("Nothing",[])]
intList = ADT "List" [("List", [TInt, TData "List"]), ("Null", [])]

callList :: [Int] -> Expr
callList [] = EVar "Null"
callList (x:xs) = callFun (EVar "List") [EIntLit x, callList xs]

-- map :: (Int -> Int) -> [Int] -> [Int]
-- map _ [] = []
-- map f (x:xs) = f x : map f xs
map' = makeFun ("map", TData "List") [("f", TArrow TInt TInt), ("x:xs", TData "List")] (
    ECase (EVar "x:xs") [
        (PData "Null" [], EVar "Null"),
        (PData "List" [PVar "x", PVar "xs"], 
        callFun (EVar "List") [
            EApply (EVar "f") (EVar "x"), 
            callFun (EVar "map") [EVar "f", EVar "xs"]
        ])
    ])

-- filter :: (Int -> Bool) -> [Int] -> [Int] 
filter' = makeFun ("filter", TData "List") [("f", TArrow TInt TBool), ("x:xs", TData "List")] (
    ECase (EVar "x:xs") [
        (PData "Null" [], EVar "Null"),
        (PData "List" [PVar "x", PVar "xs"],
        -- if f x
        EIf (EApply (EVar "f") (EVar "x")) 
        -- then x : (filter f xs)
        (callFun (EVar "List") [EVar "x", callFun (EVar  "filter") [EVar "f", EVar "xs"]]) 
        -- else (filter f xs)
        (callFun (EVar "filter") [EVar "f", EVar "xs"])
        )
    ])

-- (==) :: [Int] -> [Int] -> Bool 
equal' = makeFun ("==", TBool) [("lhs", TData "List"), ("rhs", TData "List")] (
    ECase (EVar "lhs") [
        (PData "Null" [], (ECase (EVar "rhs") [
            (PData "Null" [], EBoolLit True),
            (PVar "ys", EBoolLit False)
        ])),
        (PData "List" [PVar "x", PVar "xs"], (ECase (EVar "rhs") [
            (PData "Null" [], EBoolLit False),
            (PData "List" [PVar "y", PVar "ys"], 
                EAnd 
                    (EEq (EVar "x") (EVar "y")) 
                    (callFun (EVar "==") [(EVar "xs"), (EVar "ys")])
            )
        ]))
    ])

--  safeDiv :: Int -> Int -> Maybe Int
--  safeDiv x y = if y == 0
--     then Nothing
--     else Just (div x y)
safeDiv = makeFun ("safeDiv", TData "Maybe") [("x", TInt), ("y", TInt)] (
    EIf (EEq (EVar "y") (EIntLit 0)) 
    (EVar "Nothing") 
    (EApply (EVar "Just") (EDiv (EVar "x") (EVar "y")))
    )    

-- 1. safeDiv 7 2
result1 = 
    Program [intList, maybeInt] $
    safeDiv $ 
    callFun (EVar "safeDiv") [EIntLit 7, EIntLit 2]

-- 2. safeDiv 7 0
result2 = 
    Program [intList, maybeInt] $
    safeDiv $ 
    callFun (EVar "safeDiv") [EIntLit 7, EIntLit 0]

-- 3. [1,2] == [1,2]
result3 = 
    Program [intList] $
    equal' $
    callFun (EVar "==") [callList [1,2], callList [1,2]]

-- 4. [1,2] == [1,3]
result4 = 
    Program [intList] $
    equal' $
    callFun (EVar "==") [callList [1,2], callList [1,3]]

-- 5. [1,2] == [1]
result5 = 
    Program [intList] $
    equal' $
    callFun (EVar "==") [callList [1,2], callList [1]]

-- 6. map (\x -> x * x) [1,2,3]
result6 = 
    Program [intList] $
    map' $
    ELet ("l", callList [1,2,3]) $
    callFun (EVar "map") [ELambda ("x", TInt) (EMul (EVar "x") (EVar "x")), EVar "l"]

-- 7. filter even [1,2,3]
result7 = Program [intList] $
    filter' $
    ELet ("l", callList [1,2,3,5,6,7,89,10,100]) $
    ELet ("even", ELambda ("x", TInt) (EEq (EMod (EVar "x") (EIntLit 2)) (EIntLit 0))) $
    callFun (EVar "filter") [EVar "even", EVar "l"]