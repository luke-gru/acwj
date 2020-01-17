#include <stdarg.h>
#include <unistd.h>
#include <execinfo.h>
#include <signal.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

static void print_stacktrace(int sig) {
  void* callstack[128];
  int i, frames = backtrace(callstack, 128);
  char** strs = backtrace_symbols(callstack, frames);
  if (sig)
    fprintf(stdout, "Error: got signal %d\n", sig);
  fprintf(stdout, "\nStack trace:\n");
  for (i = 0; i < frames; ++i) {
    fprintf(stdout, "%s\n", strs[i]);
  }
  free(strs);
}

void setup_signal_handlers(void) {
  signal(SIGSEGV, print_stacktrace);
}

// Miscellaneous functions
// Copyright (c) 2019 Warren Toomey, GPL3

// Ensure that the current token is t,
// and fetch the next token. Otherwise
// throw an error
void match(int t, char *what) {
  if (Token.token == t) {
    scan(&Token);
  } else {
    fatalv("%s expected at %d:%d, got: %s", what, Line, Col, tokenname(Token.token));
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

static void print_filename(void) {
  if (Infilename) {
    fprintf(stderr, "%s:%d:%d ", Infilename, Line, Col);
  } else {
    fprintf(stderr, "<nofile>:%d:%d ", Line, Col);
  }
}

// Print out fatal messages
void fatal(char *s) {
  fatalv("%s at %d:%d", s, Line, Col);
}

void fatals(char *s1, char *s2) {
  fatalv("%s:%s at %d:%d", s1, s2, Line, Col);
}

void fatald(char *s, int d) {
  fatalv("%s: %d at %d:%d", s, d, Line, Col);
}

void fatalc(char *s, int c) {
  fatalv("%s: '%c' at %d:%d", s, c, Line, Col);
}

void fatalv(const char *fmt, ...) {
  print_filename();
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  if (fmt[strlen(fmt)-1] != '\n') {
    fprintf(stderr, "%s", "\n");
  }
  if (Outfile) fclose(Outfile);         // assembly FILE
  if (Outfilename) unlink(Outfilename); // assembly file
#ifdef NDEBUG
  exit(1);
#else
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
