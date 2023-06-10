$(V).SILENT:

TEST_DIR=tests_outputs

Compiler: Compiler.hs Syntax.hs Checker.hs Generator.hs
	ghc --make Compiler

clean:
	rm -f *.hi *.o Compiler
	rm -rf ${TEST_DIR}

setup-test-dir:
	mkdir -p ${TEST_DIR}
	rm -rf ./${TEST_DIR}/*
	cp -r tests/* ${TEST_DIR}

tests: setup-test-dir Compiler
	@$(MAKE) test-err
	@$(MAKE) test-fun
	@$(MAKE) test-opt

test-fun: setup-test-dir 
	@$(MAKE) run-test CANT=9 TEST_TYPE=fun OUT_EXT=c IN_SUFFIX='' OUT_SUFFIX=''

test-opt: setup-test-dir
	@$(MAKE) run-test CANT=9 TEST_TYPE=opt OUT_EXT=c IN_SUFFIX='' OUT_SUFFIX='_opt'

test-err: setup-test-dir
	@$(MAKE) run-test CANT=4 TEST_TYPE=err OUT_EXT=err IN_SUFFIX=err OUT_SUFFIX=err

run-test:
	@$(MAKE) setup-test-dir
	CORRECT=0; \
	SKIPPED=0; \
	for i in $$(seq 1 $(CANT)); do \
		FILE=${TEST_DIR}/ejemplo$${i}${IN_SUFFIX}; \
		OUT_FILE=${TEST_DIR}/ejemplo$${i}${OUT_SUFFIX}.${OUT_EXT}; \
		EXPECTED_FILE=tests/ejemplo$${i}${OUT_SUFFIX}.${OUT_EXT}; \
		if [ "${TEST_TYPE}" = "opt" ]; then \
			FLAG="-o"; \
		else \
			FLAG=""; \
		fi; \
		printf "Testing Compiler.hs $${FLAG} $${FILE}.fun ... "; \
		runhaskell Compiler.hs $${FLAG} "$${FILE}"; \
		ERROR_CODE=$$?; \
		if [ $$ERROR_CODE -ne 0 ]; then \
			echo "NO SE GENERO EL ARCHIVO DE SALIDA ✖ (skipped)"; \
			SKIPPED=$$((SKIPPED + 1)); \
			continue; \
		fi; \
		DIFF=$$(diff -u "$${OUT_FILE}" "$${EXPECTED_FILE}"); \
		if [ -n "$${DIFF}" ]; then \
			echo "✖"; \
			echo "$${DIFF}"; \
		else \
			echo "✔"; \
			CORRECT=$$((CORRECT + 1)); \
		fi; \
	done; \
	INCORRECT=$$((CANT - CORRECT - SKIPPED)); \
	echo "Tests passed: $${CORRECT}/$${CANT}"; \
	echo "Tests failed: $${INCORRECT}/$${CANT}"; \
	echo "Tests skipped: $${SKIPPED}/$${CANT}"; \
	exit $$INCORRECT

.PHONY: Compiler clean tests test-fun test-opt test-err setup-test-dir run-test
