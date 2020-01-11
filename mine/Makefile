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
	 ./bin/parser  test/parser/input02; \
	 ./bin/parser  test/parser/input03; \
	 ./bin/parser  test/parser/input04; \
	 ./bin/parser  test/parser/input05)

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
	 ./bin/compiler  test/compiler/pointer_scaling)


