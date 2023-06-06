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
letElimP = undefined

subst :: Name -> Expr -> Expr -> Expr
subst = undefined
