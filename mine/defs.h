#ifndef my_cc_defs_h
#define my_cc_defs_h

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

// Structure and enum definitions
// Copyright (c) 2019 Warren Toomey, GPL3
//
#define TEXTLEN		512	// Length of symbols in input
#define NSYMBOLS        1024	// Number of symbol table entries

// Tokens
enum {
  T_EOF, T_PLUS, T_MINUS, T_STAR, T_SLASH, T_INTLIT, T_SEMI, T_EQUALS,
  T_IDENT,
  T_PRINT, T_INT
};

// Token structure
struct token {
  int token;
  int intvalue;
};

// AST node types
enum {
  A_ADD, A_SUBTRACT, A_MULTIPLY, A_DIVIDE, A_INTLIT,
  A_IDENT, A_LVIDENT, A_ASSIGN
};

// Abstract Syntax Tree structure
struct ASTnode {
  int op;				// "Operation" to be performed on this tree
  struct ASTnode *left;			// Left and right child trees
  struct ASTnode *right;
  union {
    int intvalue;		// For A_INTLIT, the integer value
    int id;			// For A_IDENT, the symbol slot number
  } v;
};

// Symbol table structure
struct symtable {
  char *name;			// Name of a symbol
};

#endif
