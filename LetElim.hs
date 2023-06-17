----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de eliminación de LETs
--
-- Un LET (let x = e1 in e2) es eliminado si e1 es
-- un literal entero o booleano. En ese caso se
-- sustituyen las ocurrencias de x en e2 por e1,
-- o sea, e2[e1/x].
----------------------------------------------------------------------------

module LetElim where

import Data.List
import Syntax

-- ELIMINACION DE LETs

-- se utiliza en el proceso de optimizacion
-- se puede implementar ultimo
letElimP :: Program -> Program
letElimP (Program defs main) = Program newDefs newMain
    where newDefs = map letElimDef defs
          newMain = letElimExpr main

letElimDef :: FunDef -> FunDef
letElimDef (FunDef t n e) = FunDef t n newExpr
    where newExpr = letElimExpr e

letElimExpr :: Expr -> Expr
letElimExpr (Let t@(n,_) e1 e2) = res e1'
    where   e1' = letElimExpr e1
            e2' = letElimExpr e2
            res lit@(BoolLit b) = subst n lit e2'
            res lit@(IntLit i) = subst n lit e2'
            res e = Let t e e2'
letElimExpr (Infix op e1 e2) =  Infix op e1' e2'
    where   e1' = letElimExpr e1
            e2' = letElimExpr e2
letElimExpr (If e1 e2 e3) = If e1' e2' e3'
    where   e1' = letElimExpr e1
            e2' = letElimExpr e2
            e3' = letElimExpr e3
letElimExpr (App n es) = App n es'
    where es' = map letElimExpr es
letElimExpr e = e


-- La invocacion subst x e1 e2 implementa la sustitucion de las ocurrencias libres de x en e2 por e1
subst :: Name -> Expr -> Expr -> Expr
subst n lit e@(Var name)
    | n == name = lit
    | otherwise = e
subst n lit (Infix o e1 e2) = Infix o e1' e2'
    where   e1' = subst n lit e1
            e2' = subst n lit e2
subst n lit (If e1 e2 e3) = If e1' e2' e3'
    where   e1' = subst n lit e1
            e2' = subst n lit e2
            e3' = subst n lit e3
subst n lit (Let t e1 e2) = Let t e1' e2'
    where   e1' = subst n lit e1
            e2' = subst n lit e2
subst n lit (App name es) = App name es'
    where es' = map (subst n lit) es
subst _ _ e = e

