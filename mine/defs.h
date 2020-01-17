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
  T_COMMA, T_DOT, T_ARROW, T_COLON,
  // keywords
  T_INT, T_IF, T_ELSE, T_WHILE, T_FOR, T_BREAK, T_CONTINUE,
  T_VOID, T_CHAR, T_LONG, T_STRUCT, T_UNION, T_ENUM,
  T_SWITCH, T_CASE, T_DEFAULT, T_TYPEDEF, T_RETURN, T_EXTERN,
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
  A_IF, A_WHILE, A_BREAK, A_CONTINUE, A_FUNCTION, A_WIDEN, A_RETURN,
  A_FUNCALL, A_DEREF, A_ADDR, A_SCALE,
  A_SWITCH, A_CASE, A_DEFAULT,
  A_PREINC, A_PREDEC, A_POSTINC, A_POSTDEC,
  A_NEGATE, A_INVERT, A_LOGNOT, A_TOBOOL,
  A_LAST // sentinel
};

// 1111
#define P_PTR_BITS (0xf)

// Primitive types. The bottom 4 bits is an integer
// value that represents the level of indirection,
// e.g. 0= no pointer, 1= pointer, 2= pointer pointer etc.
enum {
    P_NONE=0,
    P_VOID=16,   //    10000
    P_CHAR=32,   //   100000
    P_INT=48,    //   110000
    P_LONG=64,   //  1000000
    P_STRUCT=96, //  1100000
    P_UNION=128, // 10000000
    P_ENUM=192,  // 11000000
    P_LAST // sentinel value
};

// Structural types
enum {
    S_VARIABLE=1, S_FUNCTION, S_PROTO, S_ARRAY
};

// Abstract Syntax Tree structure
struct ASTnode {
  int op;				// "Operation" to be performed on this tree
  int type;                             // primitive type
  int rvalue;                           // bool, true if the node is an rvalue
  struct ASTnode *left;			// Left, mid, right child trees
  struct ASTnode *mid;                  // can be NULL
  struct ASTnode *right;                // can be NULL
  struct symtable *sym;		// For many AST nodes, the pointer to
  union {
    int intvalue;		// For A_INTLIT, the integer value, for A_STRLIT the asm label
    int size;                   // For A_SCALE, the size to multiply by
  };
};

// Storage classes
enum {
    C_GLOBAL = 1,       // Globally visible symbol
    C_LOCAL,            // Locally visible symbol
    C_PARAM,            // Locally visible function parameter
    C_EXTERN,           // External globally visible symbol
    C_STRUCT,           // Struct type
    C_UNION,            // Union type
    C_ENUMTYPE,         // Enum type
    C_ENUMVAL,          // Enum value
    C_TYPEDEF,          // Aliased type name
    C_MEMBER            // Member of a struct or union
};

// Symbol table structure
struct symtable {
  char *name;                   // Name of a symbol
  int type;                     // Primitive type
  struct symtable *ctype;       // If needed, pointer to the composite type
  int stype;                    // Structural type (function, variable)
  int class;                    // Storage class for the symbol (global, local)
  union {
      int endlabel;             // for S_FUNCTION, the label to right where it's about to return to caller
      int size;                 // for S_ARRAY, number of elements.
  };
  union {
    int nelems;			// For functions, # of params
    int posn;			// For locals, the negative offset from the stack base pointer
  };
  struct symtable *next;        // next symbol in the list
  struct symtable *member;      // First param for a function, first member for struct, union or enum
};

enum {
  NOREG = -1,			// Use NOREG when the AST generation
  				// functions have no register to return
  NOLABEL = 0			// Use NOLABEL when we have no label to
    				// pass to genAST()
};

#endif
