module Parser where

import AST
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

whiteSpace :: Parser ()
whiteSpace = L.space (() <$ spaceChar) (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

braces, parens, lexeme :: Parser a -> Parser a
lexeme  = L.lexeme whiteSpace
braces = between (symbol "{") (symbol "}")
parens = between (symbol "(") (symbol ")")

symbol :: String -> Parser String
symbol = L.symbol whiteSpace

-- | 'integer' parses an integer.
integer :: Parser Int
integer = lexeme L.decimal

character :: Parser Char
character = between (symbol "'") (symbol "'") $ lexeme L.charLiteral

boolean :: Parser Bool
boolean = 
    do 
        symbol "True" 
        return True
    <|> 
    do 
        symbol "False"
        return False

getOneType :: Parser Type
getOneType = 
    do 
        symbol "Int"
        return TInt
    <|>
    do
        symbol "Char"
        return TChar
    <|>
    do 
        symbol "Bool"
        return TBool

getType :: Parser Type
getType = 
    getOneType
    <|>
    do 
        (tx,ty) <- getArrowType
        return $ TArrow tx ty

getArrowType :: Parser (Type, Type)
getArrowType = do 
    tx <- getOneType
    symbol "->"
    ty <- getType
    return (tx, ty)

-- getTypes :: Parser [Type]
-- getTypes = sepBy1 getOneType (symbol "->")


-- | 'semi' parses a semicolon.
semi :: Parser String
semi = symbol ";"

comma :: Parser String
comma = symbol ","

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["if","then","else","True","False","not","and","or", "let", "in", "case", "of", "data", "Char", "Int", "Bool"]

{- Parse an identifier
        <ident> ::= [_A-Za-z][_A-Za-z0-9']
-}
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where
    p       = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_' <|> char '\'')
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

binOp :: Parser (Expr->Expr->Expr)
binOp = 
    do 
        symbol "+" 
        return $ EAdd
    <|> do 
        symbol "-" 
        return $ ESub

----------------------------------------------------------------
-- The Program
----------------------------------------------------------------
program :: Parser Program
program = do {
    whiteSpace;
    e <- expr;
    eof;
    return $ Program [] e
}
-- program :: Parser Program
-- program =  do{whiteSpace
--              ;(tds,vds) <- manyAlternate adtDecl vDecl
--              ;eof
--              ;return $ Program tds (head vds)
--              }

-- manyAlternate :: Parser a -> Parser b -> Parser ([a],[b])
-- manyAlternate pa pb = do{as<-some pa; (as',bs') <- manyAlternate pa pb; return (as++as',bs')}
--                       <|>
--                       do{bs<-some pb; (as',bs') <- manyAlternate pa pb; return (as',bs++bs')}
--                       <|>
--                       return ([],[])

-- ----------------------------------------------------------------
-- -- ADT Declaration
-- -- data 
-- ----------------------------------------------------------------
-- adtDecl :: Parser ADT
-- adtDecl =  do{reserved "data"
--            ;t <- identifier
--            ;symbol "="
--            ;ts <- braces $ sepBy1 identifier semi
--            ;return $ ADT t ts
--            }
--            <?> "type declaration"


-- Expression
expr :: Parser Expr
expr = choice
     [
        try atomExpr,
        try binaryExpr,
        lamdaExpr,
        letExpr,
        ifExpr
     ]
     <?> "expression"

atomExpr :: Parser Expr
atomExpr = choice
        [
            try var,
            try litExpr,
            parens expr
        ]
        <?> "atomic expression"

-- Variable
var :: Parser Expr
var = EVar <$> identifier

-- Literal Expression
litExpr :: Parser Expr
litExpr = 
    EIntLit <$> integer
    <|>
    ECharLit <$> character
    <|>
    EBoolLit <$> boolean

typedVar :: Parser (String, Type)
typedVar = do
    e <- identifier
    (do
        symbol "::"
        t <- getType
        return (e, t)
        <|>
        return (e, TUnknown)
        )

lamdaExpr :: Parser Expr
lamdaExpr = do
    symbol "\\"
    (pn,pt) <- typedVar
    symbol "->"
    ELambda (pn, pt) <$> expr
    <?> "lambda expression"

letExpr :: Parser Expr
letExpr = do
    symbol "let"
    n <- identifier
    symbol "="
    (
        do
            ELambda (x,tx) e1 <- parens lamdaExpr
            symbol "::"
            (tx, ty) <- getArrowType
            symbol "in"
            ELetRec n (x, tx) (e1, ty) <$> expr
        <|>
        do
            e1 <- expr
            symbol "in"
            ELet (n, e1) <$> expr
        )
    
ifExpr :: Parser Expr
ifExpr = do
    symbol "if"
    e1 <- expr
    symbol "then"
    e2 <- expr
    symbol "else"
    EIf e1 e2 <$> expr

binaryExpr :: Parser Expr
binaryExpr = do
    e1 <- expr
    op <- binOp
    e2 <- expr
    return $ op e1 e2

main :: IO ()
main = do
    -- let input = "let x = 3 in x"
    -- let input = "\\x::Int->(\\y::Int->x)"
    -- let input = "let f=(\\var->4)::Int->Int in var"
    -- let input = "let f=(\\var->4)::Int->Int in (if var then True else False)"
    let input = "4+5"
    parseTest program input
