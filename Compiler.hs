----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo principal
----------------------------------------------------------------------------

module Main where

import Syntax
import Checker
import Generator
import LetElim

import System.Environment
import System.Console.GetOpt
import Control.Monad

data Flag = Opt | Print
 deriving (Show, Eq)

options = [ Option ['o']  ["opt"]   (NoArg Opt)
                   "optimiza los programas generados"
          , Option ['p']  ["print"]   (NoArg Print)
                   ("imprime programa optimizado " ++
                    "(solo cuando bandera opt está activa)")]

main :: IO ()
main = do
  args <- getArgs
  let header = "Uso: <progname> [OPTIONS] file\n"
  case getOpt RequireOrder options args of
    (opts,args',[]) -> do
      let optflag   = elem Opt opts
      let printflag = elem Print opts
      let filename  = last args'
      let suff      = if optflag then "_opt" else ""
      prg <- readFile (filename ++ ".fun")
      case parser prg of
        Left err  -> print err
        Right prg -> do
          let prg' = if optflag then optimize prg else prg
          when (optflag && printflag) (print prg')
          case compiler prg' of
            Right cprg -> writeFile (filename ++ suff ++ ".c") cprg
            Left  errs -> putStr errs
    (_,   _,    errs)  ->
      ioError (userError (concat errs ++
                           usageInfo header options))


compiler
  :: Program -- ast
  -> Either String String  -- produce mensajes de error o código compilado
compiler ast =
   case checkProgram ast of
     Ok        -> Right $ genProgram $ ast
     Wrong err -> Left  (unlines . map show $ err)

optimize = letElimP
