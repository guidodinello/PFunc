Compiler : Compiler.hs Syntax.hs Checker.hs Generator.hs
	ghc --make Compiler

clean :
	rm -f *.hi *.o Compiler


tests : FORCE
	mkdir -p localtests
	mkdir -p diffs
	rm -rf ./localtests/*
	cp -r tests/*.fun localtests
	for i in 1 2 3 4 5 6 7 8 9; \
         do runhaskell -w Compiler.hs localtests/ejemplo$$i; \
            runhaskell -w Compiler.hs -o localtests/ejemplo$$i; \
            runhaskell -w Compiler.hs -o -p tests/ejemplo$$i > \
              ./localtests/ejemplo$${i}.binopt ; \
	    gcc -w -o localtests/ejemplo$${i} localtests/ejemplo$${i}.c;\
            ./localtests/ejemplo$${i} > ./localtests/ejemplo$${i}.sal; \
            diff -N  tests/ejemplo$${i}.sal ./localtests/ejemplo$${i}.sal \
                 > ./diffs/$${i}sal.diff; \
            diff -N  tests/ejemplo$${i}.binopt ./localtests/ejemplo$${i}.binopt \
                 > ./diffs/$${i}binopt.diff; \
        done
	for i in 1 2 3 4 5 6 7; \
         do runhaskell -w Compiler.hs localtests/ejemplo$${i}err > \
            localtests/ejemplo$${i}err.err; \
            diff -N tests/ejemplo$${i}err.err localtests/ejemplo$${i}err.err >\
               ./diffs/ejemplo$${i}err.diff ; done

FORCE: ;
