{-# LANGUAGE TypeFamilies        #-}

module Parser where

import AST
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import EvalValue

type Parser = Parsec Void String

whiteSpace :: Parser ()
whiteSpace = L.space (() <$ spaceChar) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

braces, parens, lexeme :: Parser a -> Parser a
lexeme  = L.lexeme whiteSpace
braces = between (symbol "{") (symbol "}")
parens = between (symbol "(") (symbol ")")

symbol :: String -> Parser String
symbol = L.symbol whiteSpace

-- parses an integer.
integer :: Parser Int
integer = lexeme L.decimal

-- parse a character
character :: Parser Char
character = between (symbol "'") (symbol "'") $ lexeme L.charLiteral

-- parse boolean
boolean :: Parser Bool
boolean = 
    do 
        reserved "True" 
        return True
    <|> 
    do 
        reserved "False"
        return False

getOneType :: Parser Type
getOneType = 
    do 
        reserved "Int"
        return TInt
    <|>
    do
        reserved "Char"
        return TChar
    <|>
    do 
        reserved "Bool"
        return TBool
    <|>
    do
        t <- identifier "upper"
        return $ TData t
    <|>
        parens getType

getType :: Parser Type
getType = do
    tx <- getOneType
    (
        do
            symbol "->"
            ty <- getType
            return $ TArrow tx ty
        <|>
        return tx
        )

-- | 'semi' parses a semicolon.
semi :: Parser String
semi = symbol ";"

comma :: Parser String
comma = symbol ","

division :: Parser String
division = symbol "|"

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["if","then","else","True","False","not","and","or", "let", "in", "case", "of", "data", "Char", "Int", "Bool"]

{--
Parse an identifier
        <ident> ::= [_start][_A-Za-z0-9']
the begin alphabet must be 
    upper class if start is 'upper'
    lower class if start is 'lower'
    any letter otherwise
--}
identifier :: String -> Parser String
identifier start = (lexeme . try) (p >>= check)
    where
    sf = case start of 
        "upper" -> upperChar
        "lower" -> lowerChar
        _ -> letterChar
    p       = (:) <$> (sf <|> char '_') <*> many (alphaNumChar <|> char '_' <|> char '\'')
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- The Program
program :: Parser [Program]
program = do 
    whiteSpace
    (adts, exprs) <- manyAlternate adt expr
    eof
    return $ Prelude.map (Program adts) exprs

manyAlternate :: Parser a -> Parser b -> Parser ([a],[b])
manyAlternate pa pb = 
    do{
        as<-some pa; 
        semi;
        (as',bs') <- manyAlternate pa pb; 
        return (as++as',bs')
    }
    <|>
    do{
        bs<-some pb; 
        semi;
        (as',bs') <- manyAlternate pa pb; 
        return (as',bs++bs')
    }
    <|>
    return ([],[])

-- Variable
var :: Parser Expr
var = EVar <$> identifier "letter"

-- Literal Expression
litExpr :: Parser Expr
litExpr = 
    EIntLit <$> integer
    <|>
    ECharLit <$> character
    <|>
    EBoolLit <$> boolean
    <?> "expecting int or char or bool"

typedVar :: Parser (String, Type)
typedVar = do
    e <- identifier "letter"
    (do
        symbol ":"
        t <- getType
        return (e, t)
        <|>
        return (e, TInt)
        )
    <|> parens typedVar
    <?> "typed var"

lamdaExpr :: Parser Expr
lamdaExpr = do
    symbol "\\"
    (pn,pt) <- typedVar
    symbol "->"
    ELambda (pn, pt) <$> expr
    <?> "lambda expression"

type Bind = Expr->Expr

bindExpr :: Parser Bind
bindExpr = do
    n <- identifier "lower"
    symbol "="
    e <- expr
    (
        do
            symbol "::"
            let ELambda (x,tx) e1 = e
            TArrow tx ty <- getType
            return $ ELetRec n (x, tx) (e1, ty)
        <|> 
        do 
            return $ ELet (n,e)
        )
        -- <?> "bind expression"

letExpr :: Parser Expr
letExpr = do
    reserved "let"
    binds <- sepBy1 bindExpr comma
    reserved "in"
    e <- expr
    return $ Prelude.foldr (\b upd-> b upd) e binds
    <?> "let expression"

primaryExpr :: Parser Expr
primaryExpr = choice
    [
        try var,
        try litExpr,
        parens expr
    ]
    <?> "primary expression"

applyExpr :: Parser Expr
applyExpr = do 
    exprs <- some primaryExpr
    return $ foldl1 EApply exprs
    <?> "application"

unaryExpr :: Parser Expr
unaryExpr = do
    reserved "not"
    ENot <$> applyExpr
    <|>
    applyExpr
    <?> "unary expression"


multiplicativeExpr :: Parser Expr
multiplicativeExpr = do
    e <- unaryExpr
    (
        do
            op <- multiplicativeOp
            op e <$> multiplicativeExpr
        <|>
        return e
        )
     <?> "multiplicative expression"

type Op = Expr->Expr->Expr

multiplicativeOp :: Parser Op
multiplicativeOp = 
    do 
        symbol "*" 
        return EMul
    <|> do 
        symbol "`div`" 
        return EDiv
    <|> do
        symbol "`mod`"
        return EMod

additiveExpr :: Parser Expr
additiveExpr = do
    e <- multiplicativeExpr
    (
        do
            op <- additiveOp
            op e <$> additiveExpr
        <|>
        return e
        )
    <?> "additive expression"

additiveOp :: Parser Op
additiveOp =
    do
        symbol "+"
        return EAdd
    <|> do
        symbol "-"
        return ESub

relationalExpr :: Parser Expr
relationalExpr = do
    e <- additiveExpr
    (
        do
            op <- relationalOp
            op e <$> relationalExpr
        <|>
        return e
        )
    <?> "relational expression"

relationalOp :: Parser Op
relationalOp = 
    do
        symbol "<"
        return ELt
    <|> do
        symbol "<="
        return ELe
    <|> do
        symbol ">"
        return EGt
    <|> do
        symbol ">="
        return EGe

equalityExpr :: Parser Expr
equalityExpr = do
    e <- relationalExpr
    (
        do
            op <- equalityOp
            op e <$> equalityExpr
        <|>
        return e
        )
    <?> "equality expression"


equalityOp :: Parser Op
equalityOp = 
    do
        symbol "=="
        return EEq
    <|> do
        symbol "/="
        return ENeq

andExpr :: Parser Expr
andExpr = do
    e <- equalityExpr
    (
        do
            reserved "and"
            EAnd e <$> andExpr
        <|>
        return e
        )
        <?> "and expression"

orExpr :: Parser Expr
orExpr = do
    e <- andExpr
    (
        do
            reserved "or"
            EOr e <$> orExpr
        <|>
        return e
        )
        <?> "or expression"
    
ifExpr :: Parser Expr
ifExpr = do
    reserved "if"
    e1 <- orExpr
    reserved "then"
    e2 <- expr
    reserved "else"
    EIf e1 e2 <$> ifExpr
    <|>
    orExpr
    <?> "if expression"

caseExpr :: Parser Expr
caseExpr = do
    reserved "case"
    e0 <- expr
    reserved "of"
    pes <- braces $ sepBy1 alt comma
    return $ ECase e0 pes
    <?> "Case Expression"

alt :: Parser (Pattern, Expr)
alt = do
    p <- pattern
    symbol "->"
    e <- expr
    return $ (p, e)
    <?> "Case alternative"

pdata :: Parser Pattern
pdata = do
    x <- identifier "upper"
    patterns <- many pattern 
    return $ PData x patterns

pattern :: Parser Pattern
pattern = do
    PIntLit <$> integer
    <|>
    PCharLit <$> character
    <|>
    PBoolLit <$> boolean
    <|>
    try pdata
    <|>
    PVar <$> identifier "lower"

adt :: Parser ADT
adt = do
    reserved "data"
    t <- identifier "upper"
    symbol "="
    conss <- sepBy1 constructor division
    return $ ADT t conss

constructor :: Parser (String, [Type])
constructor = do
    cons <- identifier "upper"
    types <- many getType
    return (cons, types)


expr :: Parser Expr
expr = choice 
    [
        letExpr,
        lamdaExpr,
        ifExpr,
        caseExpr
    ]

main :: IO ()
main = getAST False

getAST :: Bool -> IO ()
getAST eval = do
    input <- getLines
    case parse program "" input of
        Left err -> putStr $ errorBundlePretty err
        Right result -> mapM_ (
            \x -> do
                putStr $ show x
                if eval then do
                    putStr " = "
                    putStrLn $ show $ EvalValue.evalProgram x
                    else return ()
                putStr "\n" 
            ) result
            

getLines :: IO String
getLines = do
    line <- getLine
    if line == ""
        then return ""
        else 
            do 
                rest <- getLines
                return $ line ++ rest