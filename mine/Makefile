INCDIR=/tmp/include
BINDIR=/tmp

install_includes:
	mkdir -p $(INCDIR)
	rsync -a system/include/. $(INCDIR)

scanner: scanner_main.c scan.c misc.c install_includes
	cc -o bin/scanner -DINCDIR=\"$(INCDIR)\" -g -rdynamic scanner_main.c scan.c misc.c

parser: expr.c interp.c parser_main.c scan.c tree.c misc.c sym.c types.c decl.c stmt.c gen.c cg.c install_includes
	cc -o bin/parser -DINCDIR=\"$(INCDIR)\" -g -rdynamic expr.c interp.c parser_main.c scan.c tree.c misc.c sym.c types.c decl.c stmt.c gen.c cg.c

compiler: expr.c interp.c codegen_main.c scan.c tree.c cg.c gen.c misc.c stmt.c sym.c decl.c types.c install_includes
	cc -o bin/compiler -DINCDIR=\"$(INCDIR)\" -g -rdynamic expr.c interp.c codegen_main.c scan.c tree.c cg.c gen.c misc.c stmt.c sym.c decl.c types.c

clean:
	rm -f *.o bin/* a.out out.s test/compiler/*.s test/compiler/*.o

test_scanner: scanner
	./bin/scanner test/scanner/input01 && \
	 ./bin/scanner test/scanner/input02 && \
	 ./bin/scanner test/scanner/input03 && \
	 ./bin/scanner test/scanner/input04 && \
	 ./bin/scanner test/scanner/input05;

test_parser: parser
	./bin/parser test/parser/input01 && \
		./bin/parser test/parser/input02 && \
		./bin/parser -T test/parser/dump_tree && \
		./bin/parser test/parser/array_decl;

test_compiler: compiler
	./bin/compiler -S test/compiler/comparisons.c && \
	 ./bin/compiler -S test/compiler/for_loop.c && \
	 ./bin/compiler -S test/compiler/if.c && \
	 ./bin/compiler -S test/compiler/main_function.c && \
	 ./bin/compiler -S test/compiler/printint.c && \
	 ./bin/compiler -S test/compiler/variables.c && \
	 ./bin/compiler -S test/compiler/while.c && \
	 ./bin/compiler -S test/compiler/types.c && \
	 ./bin/compiler -S test/compiler/function_return.c && \
	 ./bin/compiler -S test/compiler/pointers.c && \
	 ./bin/compiler -S test/compiler/global_decls.c && \
	 ./bin/compiler -S test/compiler/pointer_scaling.c && \
	 ./bin/compiler -S test/compiler/assignment_expressions.c && \
	 ./bin/compiler -S test/compiler/pointer_deref.c && \
	 ./bin/compiler -S test/compiler/basic_arrays.c && \
	 ./bin/compiler -S test/compiler/strings.c && \
	 ./bin/compiler -S test/compiler/operators.c && \
	 ./bin/compiler -S test/compiler/local_vars.c && \
	 ./bin/compiler -S test/compiler/local_array_vars.c && \
	 ./bin/compiler -S test/compiler/local_params.c && \
	 ./bin/compiler -S test/compiler/local_params2.c && \
	 ./bin/compiler -S test/compiler/prototypes.c && \
	 ./bin/compiler -S test/compiler/basic_cat.c && \
	 ./bin/compiler -S test/compiler/structs1.c && \
	 ./bin/compiler -S test/compiler/unions1.c && \
	 ./bin/compiler -S test/compiler/typedef.c && \
	 ./bin/compiler -S test/compiler/preprocessor.c && \
	 ./bin/compiler -S test/compiler/extern.c && \
	 ./bin/compiler -S test/compiler/break_continue.c;
	rm test/compiler/*.s

test_exec: compiler
	./runfile test/compiler/comparisons.c && \
			./runfile test/compiler/for_loop.c && \
			./runfile test/compiler/if.c && \
			./runfile test/compiler/main_function.c && \
			./runfile test/compiler/printint.c && \
			./runfile test/compiler/variables.c && \
			./runfile test/compiler/while.c && \
			./runfile test/compiler/types.c && \
			./runfile test/compiler/function_return.c && \
			./runfile test/compiler/pointers.c && \
			./runfile test/compiler/global_decls.c &&\
			./runfile test/compiler/pointer_scaling.c && \
			./runfile test/compiler/assignment_expressions.c && \
			./runfile test/compiler/pointer_deref.c && \
			./runfile test/compiler/basic_arrays.c && \
			./runfile test/compiler/strings.c && \
			./runfile test/compiler/operators.c && \
			./runfile test/compiler/local_vars.c && \
			./runfile test/compiler/local_array_vars.c && \
			./runfile test/compiler/local_params.c && \
			./runfile test/compiler/local_params2.c && \
			./runfile test/compiler/prototypes.c && \
			./runfile test/compiler/basic_cat.c && \
			./runfile test/compiler/structs1.c && \
			./runfile test/compiler/unions1.c && \
			./runfile test/compiler/typedef.c && \
			./runfile test/compiler/preprocessor.c && \
			./runfile test/compiler/break_continue.c;
	rm a.out

test_regression: compiler
	./test/compiler/runtests

test: test_scanner test_parser test_compiler test_exec test_regression

install: compiler install_includes
	cp ./bin/compiler $(BINDIR)
	chmox +x $(BINDIR)/compiler
