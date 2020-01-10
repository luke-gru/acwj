scanner: scanner_main.c scan.c
	cc -o bin/scanner -g scanner_main.c scan.c

parser: expr.c interp.c parser_main.c scan.c tree.c
	cc -o bin/parser -g expr.c interp.c parser_main.c scan.c tree.c

compiler: expr.c interp.c codegen_main.c scan.c tree.c cg.c gen.c misc.c stmt.c sym.c decl.c
	cc -o bin/compiler -g expr.c interp.c codegen_main.c scan.c tree.c cg.c gen.c misc.c stmt.c sym.c decl.c


clean:
	rm -f *.o bin/*

test_parser: parser
	-(./bin/parser test/parser/input01; \
	 ./bin/parser  test/parser/input02; \
	 ./bin/parser  test/parser/input03; \
	 ./bin/parser  test/parser/input04; \
	 ./bin/parser  test/parser/input05)

test_scanner: scanner
	-(./bin/scanner test/scanner/input01; \
	 ./bin/scanner  test/scanner/input02; \
	 ./bin/scanner  test/scanner/input03; \
	 ./bin/scanner  test/scanner/input04; \
	 ./bin/scanner  test/scanner/input05)
