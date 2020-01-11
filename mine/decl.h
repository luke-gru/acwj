#ifndef my_cc_decl_h
#define my_cc_decl_h

// Function prototypes for all compiler files
// Copyright (c) 2019 Warren Toomey, GPL3

// scan.c
int scan(struct token *t);
char *tokenname(int toktype);
void reject_token(struct token *t);

// tree.c
struct ASTnode *mkastnode(int op, int type, struct ASTnode *left, struct ASTnode *mid,
			  struct ASTnode *right, int intvalue);
struct ASTnode *mkastleaf(int op, int type, int intvalue);
struct ASTnode *mkuastunary(int op, int type, struct ASTnode *left, int intvalue);

// expr.c
struct ASTnode *binexpr(int n);
struct ASTnode *funcall(void);
struct ASTnode *prefix(void);

// interp.c (deprecated)
int interpretAST(struct ASTnode *n);

// gen.c
int genAST(struct ASTnode *n, int reg, int parentASTop);
void genpreamble();
void genpostamble();
void genfreeregs();
void genprintint(int reg);
void genglobsym(int slot);
int  genprimsize(int ptype);

// cg.c
void freeall_registers(void);
void cgpreamble(void);
void cgpostamble(void);
void cgfuncpreamble(int sym_id);
void cgfuncpostamble(int sym_id);
int cgloadint(int value);
int cgadd(int r1, int r2);
int cgsub(int r1, int r2);
int cgmul(int r1, int r2);
int cgdiv(int r1, int r2);
void cgprintint(int r);
int cgloadglob(int slot);
int cgstorglob(int r, int slot);
void cgglobsym(int slot);
int cgcompare_and_set(int ASTop, int r1, int r2);
int cgcompare_and_jump(int ASTop, int r1, int r2, int label);
void cglabel(int l);
void cgjump(int l);
int cgwiden(int r, int oldtype, int newtype);
int cgprimsize(int ptype);
int cgcall(int r, int func_sym);
void cgreturn(int r, int func_sym);
int cgaddress(int id);
int cgderef(int r, int type);
int cgshlconst(int r, int val);

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
void fatalv(const char *fmt, ...);
void fatald(char *s, int d);
void fatalc(char *s, int c);

// sym.c
int findglob(char *s);
int addglob(char *name, int ptype, int stype);

// decl.c
void var_declaration(int type);
struct ASTnode *function_declaration(int type);
void global_declarations(void);
int parse_type(int t);

// types.c
struct ASTnode *modify_type(struct ASTnode *tree, int rtype, int op);
int pointer_to(int ptype);
int value_at(int ptype);
const char *typename(int ptype);


#endif
