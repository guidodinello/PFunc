----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo principal
----------------------------------------------------------------------------

module Main where

import Checker
import Generator
import LetElim
import Syntax

import Control.Monad
import System.Console.GetOpt
import System.Environment

data Flag = Opt | Print
    deriving (Show, Eq)

options =
    [ Option
        ['o']
        ["opt"]
        (NoArg Opt)
        "optimiza los programas generados"
    , Option
        ['p']
        ["print"]
        (NoArg Print)
        ( "imprime programa optimizado "
            ++ "(solo cuando bandera opt está activa)"
        )
    ]

main :: IO ()
main = do
    args <- getArgs
    let header = "Uso: <progname> [OPTIONS] file\n"
    case getOpt RequireOrder options args of
        (opts, args', []) -> do
            let optflag = elem Opt opts
            let printflag = elem Print opts
            -- el nombre del archivo es el último argumento
            let filename = last args'
            let suff = if optflag then "_opt" else ""
            -- lee el .fun
            prg <- readFile (filename ++ ".fun")
            case parser prg of
                -- ParseError
                Left err -> print err
                -- ParseOk
                Right prg -> do
                    -- optimiza o no segun -o
                    let prg' = if optflag then optimize prg else prg
                    -- si se especifico -o y -p, se imprime el programa optimizado en la cli
                    when (optflag && printflag) (print prg')
                    case compiler prg' of
                        -- escribir el .c
                        Right cprg -> writeFile (filename ++ suff ++ ".c") cprg
                        -- escribir los errores de sintaxis del programa
                        Left errs -> putStr errs
        -- mostrar errores en el llamado por cli
        (_, _, errs) ->
            ioError
                ( userError
                    ( concat errs
                        ++ usageInfo header options
                    )
                )

compiler ::
    Program -> -- ast
    Either String String -- produce mensajes de error o código compilado
compiler ast =
    -- chequeo de la correctitud de la sintaxis
    case checkProgram ast of
        -- sin errores -> generar el codigo c a partir del AST
        Ok -> Right $ genProgram $ ast
        -- errores de sintaxis
        Wrong err -> Left (unlines . map show $ err)

optimize = letElimP
