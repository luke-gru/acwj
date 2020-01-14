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
void dumpAST(struct ASTnode *n, int label, int level);

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
int genglobstr(char *strvalue);

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
int cgloadglob(int slot, int ASTop);
int cgstorglob(int r, int slot);
int cgloadlocal(int slot, int ASTop);
int cgstorlocal(int r, int slot);
void cgglobsym(int slot);
int cgcompare_and_set(int ASTop, int r1, int r2);
int cgcompare_and_jump(int ASTop, int r1, int r2, int label);
void cglabel(int l);
void cgjump(int l);
int cgwiden(int r, int oldtype, int newtype);
int cgprimsize(int ptype);
void cgloadarg(int r, int type);
void cgclearargnum(void);
int cgcall(int func_sym);
void cgreturn(int r, int func_sym);
int cgaddress(int id);
int cgderef(int r, int type);
int cgstorderef(int r1, int r2, int type);
int cgshlconst(int r, int val);
void cgglobstr(int label, char *strval);
int cgbitand(int r1, int r2);
int cgbitor(int r1, int r2);
int cgbitxor(int r1, int r2);
int cgloadglobstr(int label);
int cgshl(int r1, int r2);
int cgshr(int r1, int r2);
int cgnegate(int r1);
int cginvert(int r1);
int cglognot(int r1);
int cgboolean(int r1, int ASTop, int label);
//int cggetlocaloffset(int ptype, int isparam);
int cggetlocaloffset(int slot, int isparam);
void cgresetlocals(void);

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
void debugnoisy(const char *modulename, const char *fmt, ...);

// sym.c
int findglob(char *s);
int findlocl(char *s);
int addglob(char *name, int ptype, int stype, int size);
int addlocl(char *name, int ptype, int stype, int isParam, int size);
int addparam(char *name, int ptype, int stype, int size);
int findsymbol(char *s);

// decl.c
void var_declaration(int type, int isLocal, int isParam);
struct ASTnode *function_declaration(int type);
void global_declarations(void);
int parse_type(int t);

// types.c
struct ASTnode *modify_type(struct ASTnode *tree, int rtype, int op);
int pointer_to(int ptype);
int value_at(int ptype);
const char *typename(int ptype);
int inttype(int ptype);
int ptrtype(int ptype);

#endif
