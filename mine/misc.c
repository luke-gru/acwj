#include <stdarg.h>
#include <unistd.h>
#include <execinfo.h>
#ifdef SELFHOSTED
#else
#include <signal.h>
#endif
#include <string.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Miscellaneous functions
// Copyright (c) 2019 Warren Toomey, GPL3

static int LastSig = 0;

int column() {
  if (Col < 0) { // can happen due to putback() in scan.c
    return (0);
  }
  return (Col);
}

static void print_filename(FILE *f) {
  if (Infilename) {
    if (Token.token == T_EOF) {
      fprintf(f, "%s:<EOF> ", Infilename);
    } else {
      fprintf(f, "%s:%d:%d ", Infilename, Line, column());
    }
  } else {
    if (Token.token == T_EOF) {
      fprintf(f, "%s", "<nofile>:<EOF> ");
    } else {
      fprintf(f, "<nofile>:%d:%d ", Line, column());
    }
  }
}

static void print_current_token(FILE *f) {
  fprintf(f, "at token: %s", tokenname(Token.token));
  if (Token.token == T_IDENT) {
    fprintf(f, " with value: '%s'", Text);
  } else if (Token.token == T_INTLIT) {
    fprintf(f, " with value: %d", Token.intvalue);
  }
  fprintf(f, "\n");
}

static void print_current_node(FILE *f) {
  if (!GenNode) return; // error happened before code gen
  fprintf(f, "at node with position %d:%d", GenNode->line, GenNode->col);
  fprintf(f, "\n");
}

static void print_curline(FILE *f) {
  fprintf(f, "on line:\n%s", CurLine);
#ifdef NDEBUG
#else
  print_current_token(f);
  print_current_node(f);
#endif
}

#ifdef SELFHOSTED
static void print_stacktrace(int sig) { }
#else
static void print_stacktrace(int sig) {
  void *callstack[128];
  if (sig) {
    print_filename(stdout);
    fprintf(stdout, "\n");
    print_curline(stdout);
  }

  int i, frames = backtrace(callstack, 128);
  char **strs = backtrace_symbols(callstack, frames);
  if (LastSig && LastSig == sig) {
    fprintf(stdout, "Error: got signal %s again, aborting\n", strsignal(sig));
    exit(1);
  }
  if (sig) {
    fprintf(stdout, "Error: got signal %s\n", strsignal(sig));
    LastSig = sig;
  }
  fprintf(stdout, "\nStack trace:\n");
  for (i = 0; i < frames; ++i) {
    fprintf(stdout, "%s\n", strs[i]);
  }
  free(strs);
  if (sig) {
    exit(1);
  }
}
#endif

#ifdef SELFHOSTED
void setup_signal_handlers(void) {
}
#else
void setup_signal_handlers(void) {
  signal(SIGSEGV, print_stacktrace);
}
#endif

// Ensure that the current token is t,
// and fetch the next token. Otherwise
// throw an error
void match(int t, char *what) {
  if (Token.token == t) {
    scan(&Token);
  } else {
    fatalv("%s expected at %d:%d, got: %s", what, Line, column(), tokenname(Token.token));
  }
}

// If the current token is the following, fetch the next.
int scan_if_match(int t) {
  if (Token.token == t) {
    scan(&Token);
    return (1);
  }
  return (0);
}

void semi(void) {
  match(T_SEMI, ";");
}
void ident(void) {
  match(T_IDENT, "identifier");
}
void lbrace(void) {
  match(T_LBRACE, "{");
}
void rbrace(void) {
  match(T_RBRACE, "}");
}

void lparen(void) {
  match(T_LPAREN, "(");
}
void rparen(void) {
  match(T_RPAREN, ")");
}
void comma(void) {
  match(T_COMMA, ",");
}
void dot(void) {
  match(T_DOT, ".");
}


// Print out fatal messages
void fatal(char *s) {
#ifdef SELFHOSTED
  fprintf(stderr, "%s at %d:%d\n", s, Line, column());
  exit(1);
#else
  fatalv("%s at %d:%d", s, Line, column());
#endif
}

void fatals(char *s1, char *s2) {
#ifdef SELFHOSTED
  fprintf(stderr, "%s:%s at %d:%d\n", s1, s2, Line, column());
  exit(1);
#else
  fatalv("%s:%s at %d:%d", s1, s2, Line, column());
#endif
}

void fatald(char *s, int d) {
#ifdef SELFHOSTED
  fprintf(stderr, "%s: %d at %d:%d\n", s, d, Line, column());
  exit(1);
#else
  fatalv("%s: %d at %d:%d", s, d, Line, column());
#endif
}

void fatalc(char *s, int c) {
#ifdef SELFHOSTED
  fprintf(stderr, "%s: '%c' at %d:%d\n", s, c, Line, column());
  exit(1);
#else
  fatalv("%s: '%c' at %d:%d", s, c, Line, column());
#endif
}

void fatalv(const char *fmt, ...) {
  print_filename(stderr);
  va_list ap;
  va_start(ap, fmt);
  /*fprintf(stderr, "vfprintf start\n");*/
  vfprintf(stderr, fmt, ap);
  /*fprintf(stderr, "vfprintf end\n");*/
  va_end(ap);
  if (fmt[strlen(fmt) - 1] != '\n') {
    fprintf(stderr, "%s", "\n");
  }
  if (Outfile) fclose(Outfile);         // assembly FILE
  if (Outfilename) unlink(Outfilename); // assembly file
  print_curline(stderr);
#ifdef NDEBUG
  exit(1);
#else
  fprintf(stdout, "[DEBUG] printing internal stack trace:\n");
  print_stacktrace(0);
  exit(1);
#endif
}

void debugnoisy(const char *modulename, const char *fmt, ...) {
  if (!O_debugNoisy) return;
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "[DEBUG %s]: ", modulename);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  if (fmt[strlen(fmt) - 1] != '\n') {
    fprintf(stderr, "%s", "\n");
  }
}

char *str_concat(char *str1, char *str2) {
  char *new_str;
  if ((new_str = (char*)malloc(strlen(str1)+strlen(str2)+1)) != NULL) {
    new_str[0] = '\0';   // ensures the memory is an empty string
    strcat(new_str, str1);
    strcat(new_str, str2);
  } else {
    fprintf(stderr, "malloc failed in str_append!\n");
    exit(1);
  }
  return (new_str);
}

void myassert(int expr, int line, const char *filename) {
  if (!expr) fatalv("assertion failed at %s:%d", filename, line);
}

#define CASE_PRINT(label) case label: return (#label)
#define CASE_DEFAULT_ERROR(name, val) default: \
  fatalv("Invalid " name ": %d", val)

const char *classname(int class) {
  switch (class) {
    CASE_PRINT(C_GLOBAL);
    CASE_PRINT(C_LOCAL);
    CASE_PRINT(C_PARAM);
    CASE_PRINT(C_EXTERN);
    CASE_PRINT(C_STRUCT);
    CASE_PRINT(C_UNION);
    CASE_PRINT(C_ENUMTYPE);
    CASE_PRINT(C_ENUMVAL);
    CASE_PRINT(C_TYPEDEF);
    CASE_PRINT(C_MEMBER);
    CASE_DEFAULT_ERROR("class", class);
  }
  return (NULL);
}

const char *stypename(int stype) {
  switch (stype) {
    CASE_PRINT(S_NONE);
    CASE_PRINT(S_VARIABLE);
    CASE_PRINT(S_FUNCTION);
    CASE_PRINT(S_PROTO);
    CASE_PRINT(S_ARRAY);
    CASE_DEFAULT_ERROR("stype", stype);
  }
  return (NULL);
}

char *opnames[] = {
  "A_SEQUENCE",
  "A_ASSIGN",
  "A_AS_ADD", "A_AS_SUBTRACT", "A_AS_MULTIPLY", "A_AS_DIVIDE",
  "A_TERNARY",
  "A_LOGOR", "A_LOGAND",
  "A_BITOR", "A_BITXOR", "A_BITAND",
  "A_EQ", "A_NE",
  "A_LT", "A_GT", "A_LE", "A_GE",
  "A_LSHIFT", "A_RSHIFT",
  "A_ADD", "A_SUBTRACT", "A_MULTIPLY", "A_DIVIDE", "A_MODULO", // end of mapping

  "A_INTLIT", "A_STRLIT", "A_IDENT", "A_GLUE",
  "A_IF", "A_WHILE", "A_FOR", "A_BREAK", "A_CONTINUE", "A_FUNCTION", "A_WIDEN", "A_RETURN",
  "A_FUNCALL", "A_DEREF", "A_ADDR", "A_SCALE",
  "A_SWITCH", "A_CASE", "A_DEFAULT",
  "A_PREINC", "A_PREDEC", "A_POSTINC", "A_POSTDEC",
  "A_NEGATE", "A_INVERT", "A_LOGNOT", "A_TOBOOL", "A_CAST",
  "A_GOTO", "A_LABEL", "A_EMPTY",
  "A_LAST" // sentinel
};
CASSERT(sizeof(opnames)/sizeof(char*) == A_LAST)

char *opname(int op) {
  ASSERT(op >= A_SEQUENCE && op < A_LAST);
  return (opnames[op - A_SEQUENCE]);
}

int num_spilled_args(struct symtable *func, int argnum) {
  if (func->size < 0) { // vararg function
    return (argnum - func->nelems); // args given - required args
  }
  if (argnum > 6) {
    return (argnum - 6);
  }
  return (0);
}
