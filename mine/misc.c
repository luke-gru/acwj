#include <stdarg.h>
#include <unistd.h>
#include <execinfo.h>
#include <signal.h>
#include <string.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Miscellaneous functions
// Copyright (c) 2019 Warren Toomey, GPL3

static int LastSig = 0;

int column() {
  if (Col < 0) { // can happen due to putback() in scan.c
    return 0;
  }
  return Col;
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

static void print_curline(FILE *f) {
  fprintf(f, "on line:\n%s", CurLine);
}

static void print_stacktrace(int sig) {
  if (sig) {
    print_filename(stdout);
    fprintf(stdout, "\n");
    print_curline(stdout);
  }

  void* callstack[128];
  int i, frames = backtrace(callstack, 128);
  char** strs = backtrace_symbols(callstack, frames);
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

void setup_signal_handlers(void) {
  signal(SIGSEGV, print_stacktrace);
}



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
void scan_if_match(int t) {
  if (Token.token == t) {
    scan(&Token);
  }
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


// Print out fatal messages
void fatal(char *s) {
  fatalv("%s at %d:%d", s, Line, column());
}

void fatals(char *s1, char *s2) {
  fatalv("%s:%s at %d:%d", s1, s2, Line, column());
}

void fatald(char *s, int d) {
  fatalv("%s: %d at %d:%d", s, d, Line, column());
}

void fatalc(char *s, int c) {
  fatalv("%s: '%c' at %d:%d", s, c, Line, column());
}

void fatalv(const char *fmt, ...) {
  print_filename(stderr);
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  if (fmt[strlen(fmt)-1] != '\n') {
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
  if (fmt[strlen(fmt)-1] != '\n') {
    fprintf(stderr, "%s", "\n");
  }
}

char *str_concat(char *str1, char *str2) {
  char *new_str;
  if ((new_str = malloc(strlen(str1)+strlen(str2)+1)) != NULL) {
    new_str[0] = '\0';   // ensures the memory is an empty string
    strcat(new_str, str1);
    strcat(new_str, str2);
  } else {
    fprintf(stderr, "malloc failed in str_append!\n");
    exit(1);
  }
  return new_str;
}

void myassert(int expr, int line, const char *filename) {
  if (!expr) fatalv("assertion failed at %s:%d", filename, line);
}

#define CASE_PRINT(label) case label: return #label
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
}
