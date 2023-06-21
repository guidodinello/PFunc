----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores, 
-- o la lista de errores encontrados en otro caso.   
----------------------------------------------------------------------------


module Checker where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

import Data.List
import Data.Maybe

-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type
            
instance Show Error where
 show (Duplicated      n)  = "Duplicated declaration: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (ArgNumDef   n s d)
   = "The number of parameters in the definition of "++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (ArgNumApp   n s d)
   = "The number of arguments in the application of: " ++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (Expected    ty ty')
   = "Expected: " ++ show ty ++ " Actual: " ++ show ty'


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
            | all null [duplicatesErrList, argNumParamsErrList] = undefinedErrors prg
            | otherwise = []

        -- tipo de parametro diferente de tipo de parametro en firma
        expectedErrList 
            | all null [duplicatesErrList, argNumParamsErrList, undefinedErrList] = expectedErrs prg
            | otherwise = []

        -- se puede cambiar el orden en que se muestran los errores de forma facil
        errs = duplicatesErrList ++ undefinedErrList ++ argNumParamsErrList ++ expectedErrList

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
repeatedElemsInList = go []
    where
        go _ [] = []
        go seen (x:xs)
            -- si ya aparecio, lo agrego al resultado y sigo
            | x `elem` seen = x : go seen xs
            -- si no aparecio, lo agrego a los vistos y sigo
            | otherwise = go (x:seen) xs

-- devuelve la lista de errores de tipo no declarado
undefinedErrors :: Program -> [Error]
undefinedErrors (Program defs main) = map Undefined undefinedInFns ++ map Undefined undefinedInMain
    where   
        -- nombre de funcion o parametro de funcion no definido
        definedFns = [name | (FunDef (name, _) _ _) <- defs]

        -- para cada funcion, el scope es definedFns y sus params (se incluye a ella misma en el scope permitiendo recursividad)
        -- es correcto asumiendo que en este punto no hay funciones duplicadas, sino se hubiera abortado el proceso antes
        undefinedInFns = concatMap (\(FunDef _ params expr) -> undefVarInExpr (definedFns, params) expr) defs
        -- para main, el scope son las funciones definidas
        undefinedInMain = undefVarInExpr (definedFns, []) main

-- devuelve la lista de identificadores no definidos en la expresion para el scope dado
undefVarInExpr :: ([Name],[Name]) -> Expr -> [Name]
undefVarInExpr (_, varsInScope) (Var ident) = [ident | ident `notElem` varsInScope]
undefVarInExpr _ (IntLit _) = []
undefVarInExpr _ (BoolLit _) = []
undefVarInExpr scope (Infix _ e1 e2) = undefVarInExpr scope e1 ++ undefVarInExpr scope e2
undefVarInExpr scope (If e1 e2 e3) = undefVarInExpr scope e1 ++ undefVarInExpr scope e2 ++ undefVarInExpr scope e3
undefVarInExpr scope@(fnsInScope, varsInScope) (Let (ident, _) e1 e2) = undefVarInExpr scope e1 ++ undefVarInExpr (fnsInScope, ident:varsInScope) e2
undefVarInExpr scope@(fnsInScope, _) (App ident exprs) = [ident | ident `notElem` fnsInScope] ++ concatMap (undefVarInExpr scope) exprs

-- devuelve la lista de errores de cantidad incorrecta de argumentos en las definiciones
wrongNumParamsDef :: Defs -> [Error]
wrongNumParamsDef [] = []
wrongNumParamsDef ((FunDef (name, Sig sigArg _) defArg _):xs)
    | cantSig /= cantDef = (ArgNumDef name cantSig cantDef):errors
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
    | t1 /= t2 = (TyBool, Expected t1 t2 : errors)
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