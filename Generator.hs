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
    concatMap defRepr0 defs ++
    "int main() {\n" ++
    letDefs ++
    "printf(\"%d\\n\"," ++ bodyRepr ++ "); }\n"
    where
        defRepr0 = defRepr 0
        (bodyRepr, letDefs, _) = exprRepr expr "" 0

-- -- genera el código C correspondiente a las definiciones de funciones
defRepr :: Int -> FunDef -> String
defRepr depth (FunDef (name, Sig sigTypes retType) args expr) =
    typeRepr retType ++ " _" ++ name ++ "(" ++ typedArgsString ++ "){\n" ++
    letDefs ++
    "return (" ++ bodyRepr ++ "); };\n"
    where 
        -- [TyInt, TyBool] ["x", "y"] -> "int _x, int _y"
        typedArgsString = intercalate "," $ zipWith 
            (\sigType name -> typeRepr sigType ++ " _" ++ name)
            sigTypes args
        (bodyRepr, letDefs, _) = exprRepr expr "" 0

typeRepr :: Type -> String
-- no hay booleanos en C, se usan enteros
typeRepr _ = "int"

opRepr :: Op -> String
opRepr Eq = "=="
-- definieron el show de los Op como " op " pero en los test Eq aparece como "op"
opRepr op = show op

exprRepr :: Expr -> String -> Int -> (String, String, Int)
-- el primer elemento es la representación de la expresión
-- el segundo es el string generado para las definiciones de las expresiones let (si las hay)
-- el tercero es el contador de expresiones let que sirve para su numeracion
exprRepr (Var name) defs count = ("_" ++ name, defs, count)
exprRepr (IntLit num) defs count = (show num, defs, count)
exprRepr (BoolLit value) defs count
    -- C no tiene booleanos, se usan enteros
    | value = ("1", defs, count)
    | otherwise = ("0", defs, count)
exprRepr (Infix op lExpr rExpr) defs count =
    let (lExprRepr, lDefs, lCount) = exprRepr lExpr defs count
        (rExprRepr, rDefs, rCount) = exprRepr rExpr defs lCount
    in ("(" ++ lExprRepr ++ opRepr op ++ rExprRepr ++ ")", lDefs ++ rDefs, rCount)
exprRepr (If condExpr thenExpr elseExpr) defs count =
    let (condRepr, condDefs, condCount) = exprRepr condExpr defs count
        (thenRepr, thenDefs, thenCount) = exprRepr thenExpr condDefs condCount
        (elseRepr, elseDefs, elseCount) = exprRepr elseExpr thenDefs thenCount
    in (condRepr ++ "?" ++ thenRepr ++ ":" ++ elseRepr, elseDefs, elseCount)
exprRepr (Let (varName, varType) eqExpr inExpr) defs count = 
    let (eqExprRepr, eqDefs, eqCount) = exprRepr eqExpr defs count
        (inExprRepr, inDefs, inCount) = exprRepr inExpr defs eqCount

        letId = "_let" ++ show inCount
        letDef = typeRepr varType ++ " " ++ letId ++ "(" ++ typeRepr varType ++ " _" ++ varName ++ "){\n"
        letBody = "return (" ++ inExprRepr ++ "); };\n"

    in (letId ++ "(" ++ eqExprRepr ++ ")", eqDefs ++ letDef ++ inDefs ++ letBody, inCount + 1)
exprRepr (App name exprs) defs count =
    -- h(e1, e2, ...) -> _h( exprRepr e1, exprRepr e2, ...)
    -- obs: si hay let en varias expresiones el contador debe irse acumulando dado 
    -- que las definiciones de los let estaran en el mismo scope.
    -- idem con las definiciones de las expresiones let
    let (bodyReprs, letDefs, exprCount) = mapAccum exprRepr exprs defs count
        bodyRepr = intercalate "," (map (\(r, _, _) -> r) bodyReprs)
        exprDefs = concatMap (\(_, d, _) -> d) bodyReprs
    
    in ("_" ++ name ++ "(" ++ bodyRepr ++ ")", letDefs ++ exprDefs, exprCount)

mapAccum :: 
    -- funcion de mapeo
    (Expr -> String -> Int -> (String, String, Int)) -> 
    --  lista de expresiones, string definiciones de let, contador de let
    [Expr] -> String -> Int -> 
    -- lista de representaciones de expresiones, string definiciones de let, contador de let
    ([(String, String, Int)], String, Int)
mapAccum _ [] defs count = ([], defs, count)
mapAccum f (x:xs) defs count =
    let (xRepr, xDefs, xCount) = f x defs count
        (xsReprs, xsDefs, xsCount) = mapAccum f xs xDefs xCount
    in ((xRepr, xDefs, xCount) : xsReprs, xsDefs, xsCount)
