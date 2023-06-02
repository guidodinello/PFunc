Compiler : Compiler.hs Syntax.hs LetElim.hs Checker.hs Generator.hs
	ghc --make Compiler

clean :
	rm -f *.hi *.o Compiler
