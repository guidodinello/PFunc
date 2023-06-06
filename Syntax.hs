----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de sintaxis
--
-- Incluye el Abstract Syntax Tree (tipo Program) y
-- una funcion de parsing (parser), que dado un
-- String conteniendo el codigo de un programa,
-- retorna un árbol de tipo Program si el programa
-- es sintácticamente correcto, y error en caso contrario.
-- Se incluye también un pretty printing que es
-- usado para desplegar el código optimizado.
----------------------------------------------------------------------------
{-# LANGUAGE ImportQualifiedPost #-}

module Syntax where

import Text.Parsec
import Text.Parsec.Expr hiding (Infix)
import Text.Parsec.Expr qualified as P
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

import Data.List

----------------------------------------------------------------------------
-- ABSTRACT SYNTAX TREE
----------------------------------------------------------------------------

data Type = TyInt | TyBool
        deriving (Eq)

data Program = Program Defs Expr

type Name = String

type Defs = [FunDef]

type TypedVar = (Name, Type)
type TypedFun = (Name, Sig)

data FunDef = FunDef TypedFun [Name] Expr

data Sig = Sig [Type] Type

data Expr
        = Var Name
        | IntLit Integer
        | BoolLit Bool
        | Infix Op Expr Expr
        | If Expr Expr Expr
        | Let TypedVar Expr Expr
        | App Name [Expr]

data Op
        = Add
        | Sub
        | Mult
        | Div
        | Eq
        | NEq
        | GTh
        | LTh
        | GEq
        | LEq
        deriving (Eq)

type Env = [TypedVar]

----------------------------------------------------------------------------
-- PARSER
----------------------------------------------------------------------------

parser :: String -> Either ParseError Program
parser = parse programParser ""

programParser = do
        -- ignora espacios en blanco al inicio
        m_whiteSpace
        -- lee las definiciones de funciones
        defs <- many defparser
        -- lee 'main ='
        m_reserved "main"
        m_reservedOp "="
        -- lee cuerpo del main
        body <- exprparser
        return (Program defs body)

defparser = do
        -- parsea las definiciones de funciones
        -- lee el nombre de la funcion
        fn <- m_identifier
        -- lee '::'
        m_reservedOp "::"
        -- lee los tipos de los parametros de entrada de la funcion. ([Ty, ...])
        dom <- m_parens (m_commaSep typeparser)
        -- lee '->'
        m_reservedOp "->"
        -- lee el tipo de retorno de la funcion. es unico. 'Int' o 'Bool'
        codom <- typeparser
        -- lee el nombre de la funcion, esperando por la definicion de la misma
        m_symbol fn
        -- lee los parametros de entrada de la funcion. ([x, ...])
        args <- m_parens (m_commaSep m_identifier)
        -- lee '='
        m_reservedOp "="
        -- lee el cuerpo de la funcion. el cual es una expresion
        bdy <- exprparser
        return (FunDef (fn, Sig dom codom) args bdy)

{-
foo :: ( Int , Bool ) -> Int
foo (x , b ) = if b then x * x else x + 2
PARSEA A:
FunDef ("foo", Sig [TyInt, TyBool] TyInt)
        ["x", "b"]
        (If (Var "b") (Infix Mult (Var "x") (Var "x")) (Infix Add (Var "x") (IntLit 2)))

fact :: ( Int ) -> Int
fact ( x ) = if x ==0 then 1 else x * fact (x -1)
PARSEA A:
FunDef ("fact", Sig [TyInt] TyInt)
        ["x"]
        (If (Infix Eq (Var "x") (IntLit 0)) (IntLit 1) (Infix Mult (Var "x") (App "fact" [Infix Sub (Var "x") (IntLit 1)])))
-}

typeparser =
        -- parseador de tipo, lee 'Int' o 'Bool'
        (m_reserved "Int" >> return TyInt)
                <|> (m_reserved "Bool" >> return TyBool)

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"

table =
        [ [P.Infix (m_reservedOp "==" >> return (Infix Eq)) AssocLeft]
        , [P.Infix (m_reservedOp "/=" >> return (Infix NEq)) AssocLeft]
        , [P.Infix (m_reservedOp ">" >> return (Infix GTh)) AssocLeft]
        , [P.Infix (m_reservedOp "<" >> return (Infix LTh)) AssocLeft]
        , [P.Infix (m_reservedOp ">=" >> return (Infix GEq)) AssocLeft]
        , [P.Infix (m_reservedOp "<=" >> return (Infix LEq)) AssocLeft]
        , [P.Infix (m_reservedOp "+" >> return (Infix Add)) AssocLeft]
        , [P.Infix (m_reservedOp "-" >> return (Infix Sub)) AssocLeft]
        , [P.Infix (m_reservedOp "*" >> return (Infix Mult)) AssocLeft]
        , [P.Infix (m_reservedOp "div" >> return (Infix Div)) AssocLeft]
        ]

term =
        m_parens exprparser
                <|> ( do
                        -- ‘let’ <ident> ‘::’ <type> ‘=’ <exp> ‘in’ <exp>
                        m_reserved "let"
                        vn <- m_identifier
                        m_reservedOp "::"
                        t <- typeparser
                        m_reservedOp "="
                        e <- exprparser
                        m_reserved "in"
                        b <- exprparser
                        return (Let (vn, t) e b)
                    )
                <|> ( do
                        -- 'if' <exp> 'then' <exp> 'else' <exp>
                        m_reserved "if"
                        cnd <- exprparser
                        m_reserved "then"
                        e1 <- exprparser
                        m_reserved "else"
                        e2 <- exprparser
                        return (If cnd e1 e2)
                    )
                <|> ( do
                        -- <ident> [‘(’<exp> {‘,’<exp>}‘)’]. ejemplo: funcion(1,2,3)
                        -- o tambien simplemente una variable. ejemplo: x
                        vn <- m_identifier
                        option (Var vn) (App vn <$> m_parens (m_commaSep exprparser))
                    )
                <|> (m_reserved "True" >> return (BoolLit True)) -- ‘True’
                <|> (m_reserved "False" >> return (BoolLit False)) -- ‘False’
                <|> fmap IntLit m_natural -- <int> naturales en vez de enteros?

def :: LanguageDef st
def =
        emptyDef
                { commentStart = "{-"
                , commentEnd = "-}"
                , -- caracteres con los que puede iniciar un identificador
                  identStart = letter
                , -- caracteres que pueden aparecer en un identificador
                  identLetter = alphaNum
                , -- caracteres con los que puede iniciar un operador
                  opStart = oneOf "/-<>*+=:d"
                , -- caracteres que pueden aparecer en un operador
                  opLetter = oneOf "/-<>*+=:div"
                , reservedOpNames =
                        [ "-"
                        , "<"
                        , ">"
                        , "*"
                        , "+"
                        , "="
                        , "=="
                        , ">="
                        , "<="
                        , "/="
                        , "::"
                        , "div"
                        ]
                , reservedNames =
                        [ "True"
                        , "False"
                        , "main"
                        , "if"
                        , "then"
                        , "else"
                        , "let"
                        , "in"
                        ]
                , caseSensitive = True
                }

TokenParser
        { parens = m_parens
        , identifier = m_identifier
        , symbol = m_symbol
        , reservedOp = m_reservedOp
        , reserved = m_reserved
        , commaSep = m_commaSep
        , whiteSpace = m_whiteSpace
        , natural = m_natural
        } = makeTokenParser def

----------------------------------------------------------------------------
-- PRETTY PRINTING
----------------------------------------------------------------------------

instance Show Program where
        show (Program ds e) =
                concat (map (++ "\n\n") $ map show ds) ++ "main = " ++ show e

instance Show FunDef where
        show (FunDef (f, sig) ps e) =
                f
                        ++ " :: "
                        ++ show sig
                        ++ "\n"
                        ++ f
                        ++ "("
                        ++ intercalate "," ps
                        ++ ") = "
                        ++ show e

instance Show Sig where
        show (Sig tys ty) =
                "("
                        ++ intercalate "," (map show tys)
                        ++ ")"
                        ++ " -> "
                        ++ show ty

instance Show Type where
        show TyInt = "Int"
        show TyBool = "Bool"

instance Show Expr where
        show (Var x) = x
        show (IntLit n) = show n
        show (BoolLit b) = show b
        show (Infix op e1 e2) = show e1 ++ show op ++ show e2
        show (If c e1 e2) =
                "if "
                        ++ show c
                        ++ " then "
                        ++ show e1
                        ++ " else "
                        ++ show e2
        show (Let (x, ty) e1 e2) =
                "let "
                        ++ x
                        ++ " = "
                        ++ show e1
                        ++ " in "
                        ++ show e2
        show (App f es) = f ++ "(" ++ intercalate "," (map show es) ++ ")"

instance Show Op where
        show Add = " + "
        show Sub = " - "
        show Mult = " * "
        show Div = " / "
        show Eq = " == "
        show NEq = " /= "
        show GTh = " > "
        show LTh = " < "
        show GEq = " >= "
        show LEq = " <= "
