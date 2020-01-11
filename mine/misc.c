#include <stdarg.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Miscellaneous functions
// Copyright (c) 2019 Warren Toomey, GPL3

// Ensure that the current token is t,
// and fetch the next token. Otherwise
// throw an error
void match(int t, char *what) {
  if (Token.token == t) {
    scan(&Token);
  } else {
    fatalv("%s expected on line %d, got: %s", what, Line, tokenname(Token.token));
  }
}

// Match a semicon and fetch the next token
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

// Print out fatal messages
void fatal(char *s) {
  fatalv("%s on line %d", s, Line);
}

void fatals(char *s1, char *s2) {
  fatalv("%s:%s on line %d", s1, s2, Line);
}

void fatald(char *s, int d) {
  fatalv("%s:%d on line %d", s, d, Line);
}

void fatalc(char *s, int c) {
  fatalv("%s:%c on line %d", s, c, Line);
}

void fatalv(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  if (fmt[strlen(fmt)-1] != '\n') {
    fprintf(stderr, "%s", "\n");
  }
  exit(1);
}
