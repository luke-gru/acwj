#ifndef my_cc_decl_h
#define my_cc_decl_h

// Function prototypes for all compiler files
// Copyright (c) 2019 Warren Toomey, GPL3

// scan.c
int scan(struct token *t);
char *tokenname(int toktype);
void reject_token(struct token *t);

// tree.c
struct ASTnode *mkastnode(int op, int type,
        struct ASTnode *left,
        struct ASTnode *mid,
        struct ASTnode *right,
        struct symtable *sym,
        int intvalue);
struct ASTnode *mkastleaf(int op, int type, struct symtable *sym, int intvalue);
struct ASTnode *mkuastunary(int op, int type, struct ASTnode *left, struct symtable *sym, int intvalue);
void dumpAST(struct ASTnode *n, int label, int level);

// expr.c
struct ASTnode *binexpr(int n);
struct ASTnode *funcall(void);
struct ASTnode *prefix(void);

// interp.c (deprecated)
int interpretAST(struct ASTnode *n);

// gen.c
int genAST(struct ASTnode *n, int reg, int parentASTop);
void genpreamble(void);
void genpostamble(void);
void genfreeregs(void);
void genprintint(int reg);
void genglobsym(struct symtable *sym);
int  genprimsize(int ptype);
int genglobstr(char *strvalue);

// cg.c
void freeall_registers(void);
void cgpreamble(void);
void cgpostamble(void);
void cgfuncpreamble(struct symtable *sym);
void cgfuncpostamble(struct symtable *sym);
int cgloadint(int value);
int cgadd(int r1, int r2);
int cgsub(int r1, int r2);
int cgmul(int r1, int r2);
int cgdiv(int r1, int r2);
void cgprintint(int r);
int cgloadglob(struct symtable *sym, int ASTop);
int cgstorglob(int r, struct symtable *sym);
int cgloadlocal(struct symtable *sym, int ASTop);
int cgstorlocal(int r, struct symtable *sym);
void cgglobsym(struct symtable *sym);
int cgcompare_and_set(int ASTop, int r1, int r2);
int cgcompare_and_jump(int ASTop, int r1, int r2, int label);
void cglabel(int l);
void cgjump(int l);
int cgwiden(int r, int oldtype, int newtype);
int cgprimsize(int ptype);
void cgcopyarg(int r, int argnum);
void cgclearargnum(void);
int cgcall(struct symtable *sym, int numargs);
void cgreturn(int r, struct symtable *sym);
int cgaddress(struct symtable *sym);
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
int cggetlocaloffset(struct symtable *sym);
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
char *str_concat(char *str1, char *str2);

// sym.c
struct symtable *findglob(char *s);
struct symtable *findlocl(char *s);
struct symtable *addglob(char *name, int ptype, int stype, int size);
struct symtable *addlocl(char *name, int ptype, int stype, int size);
struct symtable *addparam(char *name, int ptype, int stype, int size);
struct symtable *findsymbol(char *s);
void freeloclsyms(void);
void clear_symtable(void);

// decl.c
struct symtable *var_declaration(int type, int class);
struct ASTnode *function_declaration(int type);
void global_declarations(void);
int parse_type(int t);

// types.c
struct ASTnode *modify_type(struct ASTnode *tree, int rtype, int op);
int pointer_to(int ptype);
int value_at(int ptype);
char *typename(int ptype);
int inttype(int ptype);
int ptrtype(int ptype);

#endif
