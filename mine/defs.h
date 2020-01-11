#ifndef my_cc_defs_h
#define my_cc_defs_h

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

// Structure and enum definitions
// Copyright (c) 2019 Warren Toomey, GPL3

#define TEXTLEN		512	// Length of symbols in input
#define NSYMBOLS        1024	// Number of symbol table entries

// Tokens
enum {
  T_EOF,
  T_PLUS, T_MINUS,
  T_STAR, T_SLASH,
  T_EQ, T_NE,
  T_LT,T_GT, T_LE, T_GE,
  T_INTLIT, T_SEMI, T_ASSIGN, T_IDENT,
  T_LBRACE, T_RBRACE, T_LPAREN, T_RPAREN,
  // keywords
  T_PRINT, T_INT, T_IF, T_ELSE, T_WHILE, T_FOR, T_VOID, T_CHAR,
  T_LAST // sentinel
};


extern char *toknames[];

// Token structure
struct token {
  int token;
  int intvalue;
};

// AST node types (maps 1:1 with some tokens)
enum {
  A_ADD=1, A_SUBTRACT,
  A_MULTIPLY, A_DIVIDE,
  A_EQ, A_NE,
  A_LT, A_GT, A_LE, A_GE,
  A_INTLIT,
  A_IDENT, A_LVIDENT, A_ASSIGN, // end 1:1 mapping with T_*
  A_PRINT, A_GLUE, A_IF, A_WHILE,
  A_FUNCTION,
  A_WIDEN, // widen types node
};

// Primitive types
enum {
    P_NONE, P_VOID, P_CHAR, P_INT
};

// Structural types
enum {
    S_VARIABLE, S_FUNCTION
};

// Abstract Syntax Tree structure
struct ASTnode {
  int op;				// "Operation" to be performed on this tree
  int type;
  struct ASTnode *left;			// Left and right child trees
  struct ASTnode *mid;
  struct ASTnode *right;
  union {
    int intvalue;		// For A_INTLIT, the integer value
    int id;			// For A_IDENT, the symbol slot number
  } v;
};

// Symbol table structure
struct symtable {
  char *name;			// Name of a symbol
  int type; // Primitive type
  int stype; // Structural type (function, variable)
};

#define NOREG	-1		// Use NOREG when the AST generation
				// functions have no register to return

#endif
