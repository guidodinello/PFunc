TEST_IN_DIR=localtests/tmp
TEST_OUT_DIR=localtests

Compiler: Compiler.hs Syntax.hs Checker.hs Generator.hs
	ghc --make Compiler

clean:
	rm -f *.hi *.o Compiler

setup-test-dir:
	mkdir -p ${TEST_OUT_DIR}
	rm -rf ./${TEST_OUT_DIR}/*
	mkdir -p ${TEST_IN_DIR}
	cp -r tests/* ${TEST_IN_DIR}

tests: setup-test-dir
	@$(MAKE) run-test CANT=9 TEST_TYPE=fun TEST_EXT=c TEST_IN_SUFIX=''
	@$(MAKE) run-test CANT=9 TEST_TYPE=opt TEST_EXT=c TEST_IN_SUFIX=''
	@$(MAKE) run-test CANT=4 TEST_TYPE=err TEST_EXT=err TEST_IN_SUFIX=err

test-fun: setup-test-dir 
	@$(MAKE) run-test CANT=9 TEST_TYPE=fun TEST_EXT=c TEST_IN_SUFIX=''

test-opt: setup-test-dir
	@$(MAKE) run-test CANT=9 TEST_TYPE=opt TEST_EXT=c TEST_IN_SUFIX=''

test-err: setup-test-dir
	@$(MAKE) run-test CANT=4 TEST_TYPE=err TEST_EXT=err TEST_IN_SUFIX=err

run-test:
	@$(MAKE) setup-test-dir
	for i in $$(seq 1 $(CANT)); do \
		printf "Running $${TEST_TYPE} test $$i... "; \
		IN_FILE=${TEST_IN_DIR}/ejemplo$${i}$${TEST_IN_SUFIX}; \
		OUT_FILE=${TEST_OUT_DIR}/ejemplo$${i}$${TEST_TYPE}; \
		EXPECTED_FILE=${TEST_IN_DIR}/ejemplo$${i}$${TEST_TYPE}.$${TEST_EXT}; \
		runhaskell Compiler.hs "$${IN_FILE}" > "$${OUT_FILE}"; \
		ERROR_CODE=$$?; \
		if [ $$ERROR_CODE -ne 0 ]; then \
			echo "NO SE GENERO EL ARCHIVO DE SALIDA ✖ (skipped)"; \
			continue; \
		fi; \
		DIFF=$$(diff -u "$${OUT_FILE}" "$${EXPECTED_FILE}"); \
		if [ -n "$${DIFF}" ]; then \
			echo "✖"; \
			echo "$${DIFF}"; \
		else \
			echo "✔"; \
		fi; \
	done

.PHONY: Compiler clean tests test-fun test-opt test-err setup-test-dir run-test
