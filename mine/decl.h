#ifndef my_cc_decl_h
#define my_cc_decl_h

#define ASSERT(expr) myassert(!!(expr), __LINE__, __FILE__)

// Function prototypes for all compiler files
// Copyright (c) 2019 Warren Toomey, GPL3

// scan.c
int scan(struct token *t);
char *tokenname(int toktype);
void reject_token(struct token *t);
void reset_scanner(void);

// tree.c
struct ASTnode *mkastnode(int op, int type,
        struct symtable *ctype,
        struct ASTnode *left,
        struct ASTnode *mid,
        struct ASTnode *right,
        struct symtable *sym,
        int intvalue);
struct ASTnode *mkastleaf(int op, int type, struct symtable *ctype,
        struct symtable *sym, int intvalue);
struct ASTnode *mkastunary(int op, int type, struct symtable *ctype,
        struct ASTnode *left, struct symtable *sym, int intvalue);
void dumpAST(struct ASTnode *n, int label, int level);

// expr.c
struct ASTnode *binexpr(int n);
struct ASTnode *funcall(void);
struct ASTnode *prefix(void);
struct ASTnode *expression_list(int endtoken);
int isbinastop(int op_or_token);

// interp.c (deprecated)
int interpretAST(struct ASTnode *n);

// gen.c
int  genAST(struct ASTnode *n, int reg,
        int loopstartlabel, int loopendlabel, int parentASTop);
void genpreamble(void);
void genpostamble(void);
void genfreeregs(int keepreg);
void genprintint(int reg);
void genglobsym(struct symtable *sym);
int  genprimsize(int ptype);
int  genglobstr(char *strvalue);
int  genalign(int type, int offset, int direction);
int  genlabel(void);
void genreset(void);

// cg.c
void freeall_registers(int keepreg);
int  alloc_register(void);
void cgpreamble(void);
void cgpostamble(void);
void cgfuncpreamble(struct symtable *sym);
void cgfuncpostamble(struct symtable *sym);
int cgloadint(int value);
int cgadd(int r1, int r2);
int cgsub(int r1, int r2);
int cgmul(int r1, int r2);
int cgdiv(int r1, int r2);
int cgmod(int r1, int r2);
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
void cgjumpif(int r, int label);
void cgjumpunless(int r, int label);
int cgwiden(int r, int oldtype, int newtype);
int cgprimsize(int ptype);
void cgcopyarg(struct symtable *func, int r, int argnum);
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
int cgalign(int type, int offset, int direction);
int cgalignment(int type);
void cgswitch(int reg, int casecount, int internal_switch_dispatch_label,
    int *caselabels, int *casevals, int defaultlabel);
int cg_builtin_vararg_addr_setup(void);
void cgpush0(void);
void cgmove(int src, int dst);
void cggoto(struct symtable *sym);
void cggotolabel(struct symtable *sym);
void spill_all_regs(void);
void free_register(int reg);
void cgcomment(const char *fmt, ...);
void cgreset(void);

// stmt.c
struct ASTnode *compound_statement(int inswitch);
struct ASTnode *empty_statement(void);

// misc.c
int column(void);
void match(int t, char *what);
int scan_if_match(int t);
void semi(void);
void ident(void);
void lbrace(void);
void rbrace(void);
void lparen(void);
void rparen(void);
void comma(void);
void dot(void);
void fatal(char *s);
void fatals(char *s1, char *s2);
void fatalv(const char *fmt, ...);
void fatald(char *s, int d);
void fatalc(char *s, int c);
void debugnoisy(const char *modulename, const char *fmt, ...);
char *str_concat(char *str1, char *str2);
void setup_signal_handlers(void);
void myassert(int expr, int line, const char *filename);
const char *stypename(int stype);
const char *classname(int class);
int num_spilled_args(struct symtable *sym, int argnum);
char *opname(int op);

// sym.c
struct symtable *findglob(char *s);
struct symtable *findlocl(char *s);
struct symtable *addglob(char *name, int ptype, struct symtable *ctype, int stype, int class, int nelems);
struct symtable *addlocl(char *name, int ptype, struct symtable *ctype, int stype, int nelems);
struct symtable *addparam(char *name, int ptype, struct symtable *ctype, int stype, int nelems);
struct symtable *addstruct(char *name, int ptype, struct symtable *ctype, int stype, int nelems);
struct symtable *addunion(char *name, int ptype, struct symtable *ctype, int stype, int nelems);
struct symtable *addmember(char *name, int ptype, struct symtable *ctype, int stype, int nelems);
struct symtable *addenum(char *name, int class, int value);
struct symtable *addtypedef(char *name, int type, struct symtable *ctype,
			   int stype, int size);
struct symtable *add_or_find_label(char *s);
struct symtable *findsymbol(char *s);
struct symtable *findstruct(char *s);
struct symtable *findunion(char *s);
struct symtable *findmember(char *s);
struct symtable *findenumtype(char *name);
struct symtable *findenumval(char *name);
struct symtable *findtypedef(char *name);
int isglobalsym(struct symtable *sym);
int is_new_symbol(struct symtable *sym, int class,
                  int type, struct symtable *ctype);
int issizedsym(struct symtable *sym);
void freeloclsyms(void);
void freestaticsyms(void);
void clear_symtable(void);
void dump_sym(struct symtable *sym, FILE *f);
void dump_sym_list(struct symtable *sym, FILE *f);
void dumpsymtables(void);

// decl.c
struct symtable *function_declaration(char *name, int type, struct symtable *ctype, int class);
int declaration_list(struct symtable **ctype, int class, int et1, int et2, struct ASTnode **assign_stmts);
void global_declarations(void);
int parse_full_type(int t, struct symtable **ctype, int *class);
int parse_base_type(int t, struct symtable **ctype, int *class);
int parse_pointer_array_type(int ptype);
int parse_cast_type(struct symtable **ctype);
// Given a typedef name, return the type it represents
int type_of_typedef_fail(char *name, struct symtable **ctype);
int type_of_typedef_nofail(char *name, struct symtable **ctype);

// types.c
struct ASTnode *modify_type(struct ASTnode *tree, int rtype,
    struct symtable *rctype, int op);
int pointer_to(int ptype);
int value_at(int ptype);
char *typename(int ptype, struct symtable *ctype);
int inttype(int ptype);
int ptrtype(int ptype);
int primtype(int type);
int typesize(int type, struct symtable *ctype);

// opt.c
struct ASTnode *optimise(struct ASTnode *tree);

#endif
