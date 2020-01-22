#ifndef _STDARG_H_
#define _STDARG_H_

typedef struct Va_List {
    long varaddr;
    int argpos;
} va_list;

#define va_start(ap, lastarg) \
    ap.varaddr = __builtin_vararg_addr_setup();\
    ap.argpos = 0

// TODO: get this to be an expression (needs either better ++ processing, or
// allow comma expressions (ap.argpos++, other_expr)
#define va_arg(ap, type) ((type)(*(type*)(ap.varaddr+(8*(ap.argpos))))); \
    ap.argpos = ap.argpos + 1

// TODO: get (void)0 to parse
#define va_end(ap) 1+1

#endif
