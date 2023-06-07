----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores,
-- o la lista de errores encontrados en otro caso.
----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Checker where

import Syntax

-- se pueden agregar mas importaciones
-- en caso de ser necesario

-- TODO: eliminar import Debug
import Debug.Trace ( trace )
import qualified Data.Set as Set
import Data.List -- operador diferencia \\


import Data.List
import Data.Maybe

-- CHECKER

-- TODO: eliminar los comentarios 'no modificar'
-- no modificar
data Checked = Ok | Wrong [Error]

-- no modificar
data Error
    = Duplicated Name
    | Undefined Name
    | ArgNumDef Name Int Int
    | ArgNumApp Name Int Int
    | Expected Type Type

-- no modificar
instance Show Error where
    show (Duplicated n) = "Duplicated declaration: " ++ n
    show (Undefined n) = "Undefined: " ++ n
    show (ArgNumDef n s d) =
        "The number of parameters in the definition of "
            ++ n
            ++ " doesn't match the signature ("
            ++ show d
            ++ " vs "
            ++ show s
            ++ ")"
    show (ArgNumApp n s d) =
        "The number of arguments in the application of: "
            ++ n
            ++ " doesn't match the signature ("
            ++ show d
            ++ " vs "
            ++ show s
            ++ ")"
    show (Expected ty ty') =
        "Expected: " ++ show ty ++ " Actual: " ++ show ty'

-- recibe el AST
-- devuelve Ok o, si hubo errores de sintaxis una lista de Error. ej. [Duplicated, Undefined, etc]
checkProgram :: Program -> Checked
checkProgram prg@(Program defs main)
    | null errs =  Ok
    | otherwise =  Wrong errs
    where
        -- nombre de funcion o parametro de funcion duplicado
        duplicatesErrList = notUniqueFns defs ++ concatMap notUniqueVars defs

        -- cantparam diferente de cantparam en firma
        argNumParamsErrList
            | null duplicatesErrList = wrongNumParamsDef defs
            | otherwise = []

        -- undefined error
        undefinedErrList 
            | null duplicatesErrList && null argNumParamsErrList = undefinedErrors prg
            | otherwise = []

        -- tipo de parametro diferente de tipo de parametro en firma
        expectedErrList 
            | null duplicatesErrList && null argNumParamsErrList && null undefinedErrList = expectedErrs prg
            | otherwise = []

        -- se puede cambiar el orden en que se muestran los errores de forma facil
        errs = duplicatesErrList ++ undefinedErrList ++ argNumParamsErrList ++ expectedErrList

-- debugging
-- checkProgram (Program defs main) = trace ("Caso No controlado\n defs content: " ++ show defs ++ "\nexpressions:" ++ show main) Wrong []

-- Check the uniqueness of function names (Name, _) for TypedFun in FunDef
notUniqueFns :: [FunDef] -> [Error]
notUniqueFns funDefs = map Duplicated $ repeatedElemsInList funNames
    where
        funNames = [name | (FunDef (name, _) _ _) <- funDefs]
-- Check the uniqueness of variables in [names]
notUniqueVars :: FunDef -> [Error]
notUniqueVars (FunDef _ varNames _) = map Duplicated $ repeatedElemsInList varNames

-- devuelve [xi] tal que xi esta repetido en xs, tantas veces como este repetido
-- ejemplo: [1,2,3,4,5,1,2,2] -> [1,2,2]
-- se usa para chequear 1. si hay funciones con el mismo nombre 2. en una funcion, parametros con el mismo nombre
repeatedElemsInList :: Eq a => [a] -> [a]
repeatedElemsInList [] = []
repeatedElemsInList (x:xs) = if x `elem` xs then x : repeatedElemsInList xs else repeatedElemsInList xs

-- devuelve la lista de errores de tipo no declarado
undefinedErrors :: Program -> [Error]
undefinedErrors (Program defs main) = concatMap undefinedVars defs ++ undefinedFnsInEachFn ++ undefinedFnsInMain
    where   -- nombre de funcion o parametro de funcion no definido
            definedFns = [name | (FunDef (name, _) _ _) <- defs]
            -- obtiene las f_i usadas dentro de f_j pero f_i no esta definida
            usedFns = concat [getApps body | (FunDef _ _ body) <- defs]
            undefinedFnsInEachFn = map Undefined $ usedFns \\ definedFns
            -- obtiene las f_i usadas dentro del main pero f_i no esta definida
            undefinedFnsInMain = map Undefined $ (getApps main) \\ definedFns

-- devuelve la lista de errores de variable usada dentro de funcion pero no es argumento
-- capaz hay que cambiarla, que pasa por ejemplo con un let x
-- f(a,b) = a+b ... let x  , esto devolveria x como undefined
undefinedVars :: FunDef -> [Error]
undefinedVars (FunDef _ argNames expr) = map Undefined undefinedNames
    where
        undefinedNames = [name | name <- getVars expr, name `notElem` argNames]

-- devuelve la lista de Identificadores de variable presentes en la Expresion
getVars :: Expr -> [String]
getVars (Var name) = [name]
getVars (IntLit _) = []
getVars (BoolLit _) = []
getVars (Infix _ e1 e2) = getVars e1 ++ getVars e2
getVars (If e1 e2 e3) = getVars e1 ++ getVars e2 ++ getVars e3
getVars (Let _ e1 e2) = getVars e1 ++ getVars e2
getVars (App _ es) = concatMap getVars es

-- devuelve la lista de errores de cantidad incorrecta de argumentos en las definiciones
wrongNumParamsDef :: Defs -> [Error]
wrongNumParamsDef [] = []
wrongNumParamsDef ((FunDef (name, Sig sigArg _) defArg _):xs)
    | cantSig /= cantDef = (ArgNumDef name cantDef cantSig):errors
    | otherwise = errors
    where errors = wrongNumParamsDef xs
          cantSig = length sigArg
          cantDef = length defArg

-- devuelve la cantidad de argumentos en la signatura para la funcion con nombre n
getDefArgCount :: Defs -> Name -> Int
getDefArgCount [] _ = 0
getDefArgCount ((FunDef (name, Sig sigArg _) _ _):xs) n
    | n == name = length sigArg
    | otherwise = getDefArgCount xs n

-- devuelve los errores de tipos esperados y de cant de parametros en el programa
expectedErrs ::  Program -> [Error]
expectedErrs (Program defs main) = errsDef ++ errsMain
    where   errsDef = expectedErrsDefs defs defs
            (_,errsMain) = expectedErrsExpr main defs []

-- devuelve los errores de tipos esperados en las definiciones
expectedErrsDefs :: Defs -> Defs -> [Error]
expectedErrsDefs [] _ = []
expectedErrsDefs ((FunDef (name, Sig argsTypes ret) argsName expr):xs) defs
    | ret /= exprType = errors ++ errs ++ [Expected ret exprType]
    | otherwise = errors ++ errs
    where   env = zip argsName argsTypes
            (exprType, errs) = expectedErrsExpr expr defs env
            errors = expectedErrsDefs xs defs

-- devuelve los errores de tipos esperados para las expresiones
expectedErrsExpr :: Expr -> Defs -> Env -> (Type, [Error])
-- variable -> busco en el ambiente
expectedErrsExpr (Var _) _ [] = (TyInt,[]) -- no deberia pasar -> antes es undefined
expectedErrsExpr v@(Var name) defs ((n,t):xs)
    | name == n = (t,[])
    | otherwise = expectedErrsExpr v defs xs
-- literal entero
expectedErrsExpr (IntLit _) _ _ = (TyInt, [])
-- literal booleano
expectedErrsExpr (BoolLit _) _ _ = (TyBool, [])
-- operador
expectedErrsExpr (Infix op e1 e2) defs env
    | isArithmetic && t1 /= TyInt = (TyInt, errors ++ [Expected TyInt t1])
    | isArithmetic && t2 /= TyInt = (TyInt, errors ++ [Expected TyInt t2])
    | isArithmetic = (TyInt, errors)
    | t1 /= t2 = (TyBool, errors ++ [Expected t1 t2])
    | otherwise = (TyBool, errors)
    where   (t1,errs1) = expectedErrsExpr e1 defs env
            (t2,errs2) = expectedErrsExpr e2 defs env
            errors = errs1 ++ errs2
            isArithmetic = isArithmeticOperator op
-- condicional
expectedErrsExpr (If e1 e2 e3) defs env = (t2, errs1 ++ errsCond ++ errs2 ++ errs3 ++ errsRes)
    where   (t1,errs1) = expectedErrsExpr e1 defs env
            (t2,errs2) = expectedErrsExpr e2 defs env
            (t3,errs3) = expectedErrsExpr e3 defs env
            errsCond
                | t1 /= TyBool = [Expected TyBool t1]
                | otherwise = []
            errsRes
                | t2 /= t3 = [Expected t2 t3]
                | otherwise = []
-- let
expectedErrsExpr (Let var@(_,t) e1 e2) defs env = (t2, errs1 ++ errsLet ++ errs2)
    where   (t1,errs1) = expectedErrsExpr e1 defs env
            (t2,errs2) = expectedErrsExpr e2 defs (var:env)
            errsLet
                | t /= t1 = [Expected t t1]
                | otherwise = []
-- funcion
expectedErrsExpr (App n args) defs env = (t, errsParams ++ errsArgs)
    where   (Sig argTypes t) = getFnSig defs n
            cantSig = length argTypes
            cantApp = length args
            errsParams
                | cantSig /= cantApp = [ArgNumApp n cantSig cantApp]
                | otherwise = []
            argActual = map (\e -> expectedErrsExpr e defs env) args
            errsArgs = concat $ zipWith getAppArgErrors argTypes argActual

-- dada un tipo esperado y el resultado de evaluar expectedErrsExpr retorna la lista de errores, incluyendo si no coincide el error de tipo esperado para la expresion evaluada
getAppArgErrors :: Type -> (Type, [Error]) -> [Error]
getAppArgErrors exp (t,err)
    | exp /= t = err ++ [Expected exp t]
    | otherwise = err

-- retorna la signatura para la defincion con nombre name                
getFnSig :: Defs -> Name -> Sig
getFnSig [FunDef (name, sig) _ _] n = sig --- por defecto retorno la ultima, sino fuera deberia estar en undefined
getFnSig ((FunDef (name, sig) _ _):xs) n
    | n == name = sig
    | otherwise = getFnSig xs n

-- retorn true si el operador es aritmetico
isArithmeticOperator :: Op -> Bool
isArithmeticOperator Add = True
isArithmeticOperator Sub = True
isArithmeticOperator Mult = True
isArithmeticOperator Div = True
isArithmeticOperator _ = False

-- devuelve la lista de nombres de las Aplicaciones presentes en la Expresion
getApps :: Expr -> [Name]
getApps (Var _) = []
getApps (IntLit _) = []
getApps (BoolLit _) = []
getApps (Infix _ e1 e2) = getApps e1 ++ getApps e2
getApps (If e1 e2 e3) = getApps e1 ++ getApps e2 ++ getApps e3
getApps (Let _ e1 e2) = getApps e1 ++ getApps e2
getApps (App name es) = name : concatMap getApps es

