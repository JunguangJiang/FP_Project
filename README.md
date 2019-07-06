# readme
使用Stack进行管理。
在package.yaml中加入了所需的包依赖。
```
- tasty
- tasty-hunit
- monad-loops
- parser-combinators
- megaparsec
```

运行 stack test 进行自动测试。
运行stack ghci进入使用环境。

#### ADT功能展示
在Show.hs下定义了我实现的ADT机制下的Maybe Int 和[Int]类，其中计算程序分别为result[1-7], 可以通过
```
EvalValue.evalValue result1
```
查看程序结果。

可以通过
```
EvalType.evalType result1
```
查看程序的结果类型。

具体的核心语法以及ADT相关的模式匹配、定义、声明语法和助教所提供的文档一致，此处不再赘述。

#### Parser及REPL功能展示
- 用；表示一个语句的完成，用';'分隔，用空行表示结束
- 使用Parser.main进入Parser环境，输入下述示例即可
- 使用REPL.lines进入REPL多行环境，输入下述示例即可

##### 使用示例
```haskell
True;
40+2;
'@'/='@';
if False then 42 else 233;
\x -> x+1;
let even = (\(x:Int) -> x `mod` 2 == 0) in even 42;
let fact = (\x -> if x == 0 then 1 else x * fact (x-1)) :: Int -> Int in fact 5;

data MaybeInt = Just Int | Nothing;
let safeDiv = \x->\y->
    if y == 0 
        then Nothing 
        else Just (x `div` y)
    in safeDiv 7 2;

data IntList = List Int IntList | Null;
let equal = \xl -> \(yl:IntList) ->
    case yl of {
        Null-> case xl of{
            Null -> True,
            xs -> False
        },
        List y ys -> case xl of {
            Null -> False,
            List x xs -> x == y and (equal xs ys)
        }
    } :: (IntList->IntList->Bool), 
    x = List 1 (List 2 Null),
    y = List 1 (List 2 Null)
    in equal x y;
    
data IntList = List Int IntList | Null;
let map = \f -> \(xl:IntList)-> 
    case xl of {
        Null-> Null,
        List x xs -> List (f x) (map f xs)
    }:: ((Int->Int) -> IntList -> IntList), 
    l = List 1 (List 2 (List 3 Null))
    in map (\x->x*x) l;

data IntList = List Int IntList | Null;
let filter = \f -> \(xl:IntList)-> 
    case xl of {
        Null-> Null,
        List x xs -> if (f x) 
            then List x (filter f xs)
            else (filter f xs)
    } :: ((Int->Bool)->IntList -> IntList), 
    l = List 1 (List 2 (List 3 Null))
    in filter (\x->(x `mod` 2) == 0) l;
```

##### 注意事项
- 尚未支持自动类型推断，因此在定义lambda表达式时，需要指明变量类型
- 当lambda表达式有::后缀时，会被解释成LetRec,否则被解释成Let

## 功能及原理
### 求AST的实例例的类型
具体见作业要求文档。
其中ELetRec f (x, tx) (e1, ty) e2递归实现的关键在于在计算e1类型的时候，需要将{key=f,value=tx->ty}加入context，然后检查e1的计算结果是否和ty相等。
### 对AST的实例例求值
除了基本数据值VBool,VInt,VChar外还定义了
* VClosure：存储函数名到函数体的绑定，不直接求值的原因是为了支持递归调用。
* VAdtFun、VAdt：代数数据类型相关，见后文。

### 实现代数数据类型的声明
在EvalType中，将Program中的adts（代数数据类型声明）转化成从构造函数到参数类型、类型名的映射函数，此后遇到代数数据类型的构造时，通过findVar寻找参数类型即可。

在EvalValue中，将Program中的每个代数数据类型声明转成构造函数到VAdt或者VAdtFun的映射，其中VAdtFun需要接受足够数量的参数才能成为VAdt。

### 实现代数数据类型的构造函数
使用EApply (EVar "constructor") param1进行代数数据类型的构造，其中"constructor"是代数数据类型构造函数的名字，param1是构造函数的第一个参数，若要增加多个参数，需要EApply多次。
### 模式匹配语句句的支持
ECase e0 [(p1, e1),(p2, e2),...]

在EvalType中会首先根据模式pi验证其与e0是否类型一致，同时计算出一个上下文环境context（包含pi中出现的变量），然后用context计算ei的类型，并检验e1-en的类型是否一致。由于pi也可以是ECase语句，所以该过程是支持递归的。

在EvalValue中会检查模式pi和ei的值是否相等，并将pi中的变量加入ei求值的上下文。找到首个匹配的模式pk后，计算ek,不再计算此后的模式值。

### 文法设计和 parser
####  支持的语法
- True/False/整数/字符。
- not and or
- + - * \`div\` \`mod\` == /= < > <= >=
- if e1 then e2 else e3
- \x->e, \(x:Bool)->e
- let n1 = e1, n2=e2... in e (支持在单个let 语句句中进⾏行行多个绑定)
- let f = (\x -> e1) :: tx -> ty in e2
- case e0 of {p1->e1, p2->e2}
- data T = cons1 [params1] | cons2 [params2]
- ⽀持在let语句中使⽤用模式匹配

#### 技术细节
- 在模式匹配时用首字母大小写区分变量和代数数据类型的构造函数。例如Nothing是构造函数，nothing则是一个变量。
- 多条语句通过';'进行分隔。首先将其中出现的所有data语句转成ADT，然后在ADT环境下，对其余语句逐一进行parse。


### REPL
只实现一个非常简单的REPL，不支持状态的记忆，主要用于前述功能的展示。
可以输入多行语句（用';'分隔，用空行表示结束），输出结果的值与类型。
