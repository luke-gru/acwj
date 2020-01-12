scanner: scanner_main.c scan.c misc.c
	cc -o bin/scanner -g scanner_main.c scan.c misc.c

parser: expr.c interp.c parser_main.c scan.c tree.c misc.c sym.c types.c decl.c stmt.c gen.c cg.c
	cc -o bin/parser -g expr.c interp.c parser_main.c scan.c tree.c misc.c sym.c types.c decl.c stmt.c gen.c cg.c

compiler: expr.c interp.c codegen_main.c scan.c tree.c cg.c gen.c misc.c stmt.c sym.c decl.c types.c
	cc -o bin/compiler -g expr.c interp.c codegen_main.c scan.c tree.c cg.c gen.c misc.c stmt.c sym.c decl.c types.c

clean:
	rm -f *.o bin/* a.out out.s

test_scanner: scanner
	-(./bin/scanner test/scanner/input01; \
	 ./bin/scanner  test/scanner/input02; \
	 ./bin/scanner  test/scanner/input03; \
	 ./bin/scanner  test/scanner/input04; \
	 ./bin/scanner  test/scanner/input05)

test_parser: parser
	-(./bin/parser test/parser/input01; \
	 ./bin/parser test/parser/input02; \
	 ./bin/parser -T test/parser/dump_tree; \
	 ./bin/parser test/parser/array_decl)

test_compiler: compiler
	-(./bin/compiler test/compiler/comparisons; \
	 ./bin/compiler  test/compiler/for_loop; \
	 ./bin/compiler  test/compiler/if; \
	 ./bin/compiler  test/compiler/main_function; \
	 ./bin/compiler  test/compiler/printint; \
	 ./bin/compiler  test/compiler/variables; \
	 ./bin/compiler  test/compiler/while; \
	 ./bin/compiler  test/compiler/types; \
	 ./bin/compiler  test/compiler/function_return; \
	 ./bin/compiler  test/compiler/pointers; \
	 ./bin/compiler  test/compiler/global_decls; \
	 ./bin/compiler  test/compiler/pointer_scaling; \
	 ./bin/compiler  test/compiler/assignment_expressions; \
	 ./bin/compiler  test/compiler/pointer_deref; \
	 ./bin/compiler  test/compiler/basic_arrays; \
	 ./bin/compiler  test/compiler/strings)

test_exec:
	-(./runfile test/compiler/comparisons; \
	  ./runfile  test/compiler/for_loop; \
	  ./runfile  test/compiler/if; \
	  ./runfile  test/compiler/main_function; \
	  ./runfile  test/compiler/printint; \
	  ./runfile  test/compiler/variables; \
	  ./runfile  test/compiler/while; \
	  ./runfile  test/compiler/types; \
	  ./runfile  test/compiler/function_return; \
	  ./runfile  test/compiler/pointers; \
	  ./runfile  test/compiler/global_decls; \
	  ./runfile  test/compiler/pointer_scaling; \
	  ./runfile  test/compiler/assignment_expressions; \
	  ./runfile  test/compiler/pointer_deref; \
	  ./runfile  test/compiler/basic_arrays; \
	  ./runfile  test/compiler/strings)

test: test_scanner test_parser test_compiler test_exec
