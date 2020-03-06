.PHONY: all
all:
	erl -make

.PHONY: check 
check: all
	erl -noinput -s tests run_tests

.PHONY: clean
clean:
	rm -rf *.beam
