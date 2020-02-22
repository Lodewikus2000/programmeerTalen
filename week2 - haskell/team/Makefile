.PHONY: run
run: tests
	./tests

tests: tests.hs SudokuSolver.hs
	stack ghc -- $<

.PHONY: clean
clean:
	rm -f tests
	rm -f *.hi
	rm -f *.o
