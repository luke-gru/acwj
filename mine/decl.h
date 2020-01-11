#ifndef my_cc_decl_h
#define my_cc_decl_h

// Function prototypes for all compiler files
// Copyright (c) 2019 Warren Toomey, GPL3

// scan.c
int scan(struct token *t);
char *tokenname(int toktype);

// tree.c
struct ASTnode *mkastnode(int op, struct ASTnode *left, struct ASTnode *mid,
			  struct ASTnode *right, int intvalue);
struct ASTnode *mkastleaf(int op, int intvalue);
struct ASTnode *mkuastunary(int op, struct ASTnode *left, int intvalue);
struct ASTnode *binexpr(int n);
int interpretAST(struct ASTnode *n);

// gen.c
int genAST(struct ASTnode *n, int reg, int parentASTop);
void genpreamble();
void genpostamble();
void genfreeregs();
void genprintint(int reg);
void genglobsym(char *s);

// cg.c
void freeall_registers(void);
void cgpreamble(void);
void cgfuncpreamble(char *name);
void cgfuncpostamble();
int cgloadint(int value);
int cgadd(int r1, int r2);
int cgsub(int r1, int r2);
int cgmul(int r1, int r2);
int cgdiv(int r1, int r2);
void cgprintint(int r);
int cgloadglob(char *identifier);
int cgstorglob(int r, char *identifier);
void cgglobsym(char *sym);
int cgcompare_and_set(int ASTop, int r1, int r2);
int cgcompare_and_jump(int ASTop, int r1, int r2, int label);
void cglabel(int l);
void cgjump(int l);

// stmt.c
struct ASTnode *compound_statement(void);

// misc.c
void match(int t, char *what);
void semi(void);
void ident(void);
void lbrace(void);
void rbrace(void);
void lparen(void);
void rparen(void);
void fatal(char *s);
void fatals(char *s1, char *s2);
void fatald(char *s, int d);
void fatalc(char *s, int c);

// sym.c
int findglob(char *s);
int addglob(char *name);

// decl.c
void var_declaration(void);
struct ASTnode *function_declaration(void);

#endif
