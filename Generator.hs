----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de generación de código C
--
-- Se debe implementar la función genProgram,
-- que dado un AST que representa un programa válido
-- genera el código C correspondiente.
----------------------------------------------------------------------------

module Generator where

import Syntax

-- se pueden agregar mas importaciones
-- en caso de ser necesario

import Data.List

-- CODE GENERATOR

-- genera el código C correspondiente al AST
genProgram :: Program -> String
genProgram (Program defs expr) = 
    "#include <stdio.h>\n" ++
    concatMap defRepr defs ++
    "int main() {\n" ++
    "printf(\"%d\\n\"," ++ exprRepr expr ++ "); }\n"

-- -- genera el código C correspondiente a las definiciones de funciones
defRepr :: FunDef -> String
defRepr (FunDef (name, Sig sigTypes retType) args expr) =
    typeRepr retType ++ " _" ++ name ++ "(" ++ typedArgsString ++ "){\n" ++
    "return (" ++ exprRepr expr ++ "); };\n"
    where
        typedArgsString = intercalate "," $ zipWith 
                (\sigType name -> typeRepr sigType ++ " _" ++ name)
                sigTypes args

typeRepr :: Type -> String
-- no hay booleanos en C, se usan enteros
typeRepr _ = "int"

exprRepr :: Expr -> String
exprRepr (Var name) = "_" ++ name
exprRepr (IntLit num) = show num
exprRepr (BoolLit value)
    | value = "1"
    | otherwise = "0"
exprRepr (Infix op lExpr rExpr) = "(" ++ exprRepr lExpr ++ show op ++ exprRepr rExpr ++ ")"
-- _b?(_x * _x):(_x + 2) 
exprRepr (If condExpr thenExpr elseExpr) =  
    exprRepr condExpr ++ "?(" ++ exprRepr thenExpr ++ "):(" ++ exprRepr elseExpr ++ ")"
{-
(let x :: Int = y in x)
              + (let y :: Int = x + y in y)
int _let0(int _x){
return (_x); };
int _let1(int _y){
return (_y); };
return ((_let0(_y) + _let1((_x + _y))));
-}
exprRepr (Let typedVar eqExpr inExpr) = undefined
exprRepr (App name exprs) = "_" ++ name ++ "(" ++ intercalate "," (map exprRepr exprs) ++ ")"

