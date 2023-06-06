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

-- TODO: eliminar import Debug
import Debug.Trace ( trace )
import qualified Data.Set as Set


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
checkProgram (Program defs exprs) = if null errs then Ok else Wrong errs
    where
        -- nombre de funcion o parametro de funcion duplicado
        duplicatesErrList = notUniqueFns defs ++ concatMap notUniqueVars defs

        -- nombre de funcion o parametro de funcion no definido
        fnsUsedInMain = getApps exprs
        undefinedErrList = concatMap undefinedVars defs ++ undefinedFns defs ++ undefinedFnsInMain exprs
        -- cantparam diferente de cantparam en firma
        argNumParamsErrList = wrongNumParamsDef ++ wrongNumParamsApp
        -- tipo de parametro diferente de tipo de parametro en firma
        expectedErrList = expectedErrs

        -- se puede cambiar el orden en que se muestran los errores de forma facil
        errs = duplicatesErrList ++ undefinedErrList ++ argNumParamsErrList ++ expectedErrList

-- debugging
-- checkProgram (Program defs exprs) = trace ("Caso No controlado\n defs content: " ++ show defs ++ "\nexpressions:" ++ show exprs) Wrong []


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

-- devuelve la lista de Errores de funcion usada y no definida
undefinedFns :: [FunDef] -> [Error]
-- undefinedFns funDefs = map Undefined undefinedNames
undefinedFns = undefined

-- devuelve la lista de errores de variable usada dentro de funcion pero no es argumento
-- capaz hay que cambiarla, que pasa por ejemplo con un let x
-- f(a,b) = a+b ... let x  , esto devolveria x como undefined
undefinedVars :: FunDef -> [Error]
undefinedVars (FunDef _ argNames expr) = map Undefined undefinedNames
    where
        undefinedNames = [name | name <- getVars expr, name `notElem` argNames]

-- devuelve la lista de variables presentes en la Expresion
getVars :: Expr -> [String]
getVars (Var name) = [name]
getVars (IntLit _) = []
getVars (BoolLit _) = []
getVars (Infix _ e1 e2) = getVars e1 ++ getVars e2
getVars (If e1 e2 e3) = getVars e1 ++ getVars e2 ++ getVars e3
getVars (Let _ e1 e2) = getVars e1 ++ getVars e2
getVars (App _ es) = concatMap getVars es


getApps = undefined
undefinedFnsInMain = undefined
wrongNumParamsDef = undefined
wrongNumParamsApp = undefined
expectedErrs = undefined