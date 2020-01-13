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
  T_EOF=0,
  // beginning of operators (precedence table)
  // binary operators
  T_ASSIGN, T_LOGOR, T_LOGAND, // low prec operators come first
  T_BITOR, T_BITXOR, T_AMPER,
  T_EQ, T_NE,
  T_LT, T_GT, T_LE, T_GE,
  T_LSHIFT, T_RSHIFT,
  T_PLUS, T_MINUS, T_STAR, T_SLASH,

  // other operators
  T_INC, T_DEC, T_INVERT, T_LOGNOT,
  // end of operators (precedence table)

  T_INTLIT, T_SEMI, T_IDENT, T_STRLIT,
  T_LBRACE, T_RBRACE, T_LPAREN, T_RPAREN, T_LBRACKET, T_RBRACKET,
  T_COMMA,
  // keywords
  T_INT, T_IF, T_ELSE, T_WHILE, T_FOR, T_VOID, T_CHAR, T_LONG, T_RETURN,
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
  A_ASSIGN=T_ASSIGN, A_LOGOR, A_LOGAND,
  A_BITOR, A_BITXOR, A_BITAND,
  A_EQ, A_NE,
  A_LT, A_GT, A_LE, A_GE,
  A_LSHIFT, A_RSHIFT,
  A_ADD, A_SUBTRACT, A_MULTIPLY, A_DIVIDE, // end of mapping

  A_INTLIT, A_STRLIT, A_IDENT, A_GLUE,
  A_IF, A_WHILE, A_FUNCTION, A_WIDEN, A_RETURN,
  A_FUNCALL, A_DEREF, A_ADDR, A_SCALE,
  A_PREINC, A_PREDEC, A_POSTINC, A_POSTDEC,
  A_NEGATE, A_INVERT, A_LOGNOT, A_TOBOOL,
  A_LAST // sentinel
};

// Primitive types
enum {
    P_NONE, P_VOID, P_CHAR, P_INT, P_LONG,
    P_VOIDPTR, P_CHARPTR, P_INTPTR, P_LONGPTR,
    P_LAST // sentinel value
};

// Structural types
enum {
    S_VARIABLE, S_FUNCTION, S_ARRAY
};

// Abstract Syntax Tree structure
struct ASTnode {
  int op;				// "Operation" to be performed on this tree
  int type;                             // primitive type
  int rvalue;                           // bool, true if the node is an rvalue
  struct ASTnode *left;			// Left, mid, right child trees
  struct ASTnode *mid;                  // can be NULL
  struct ASTnode *right;                // can be NULL
  union {
    int intvalue;		// For A_INTLIT, the integer value
    int id;			// For A_IDENT or A_FUNCTION, the symbol slot number
    int size;                   // For A_SCALE, the size to multiply by
  } v;
};

// Storage classes
enum {
    C_GLOBAL = 1,       // Globally visible symbol
    C_LOCAL             // Locally visible symbol
};

// Symbol table structure
struct symtable {
  char *name;                   // Name of a symbol
  int type;                     // Primitive type
  int stype;                    // Structural type (function, variable)
  int class;                    // Storage class for the symbol (global, local)
  int endlabel;                 // for S_FUNCTION, the label to right where it's about to return to caller
  int size;                     // for S_ARRAY, number of elements
  int posn;                     // For locals, the negative offset from the stack base pointer (rbp)
};

#define NOREG	-1		// Use NOREG when the AST generation
				// functions have no register to return

#endif
