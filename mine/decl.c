#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of declarations
// Copyright (c) 2019 Warren Toomey, GPL3

// Parse the current token and
// return a primitive type enum value
// TODO: allow type to be an array type without size given, like for
// parameters: `char[] fmt`
int parse_type(int t) {
  int type = P_NONE;
  if (t == T_CHAR) type = P_CHAR;
  if (t == T_INT)  type = P_INT;
  if (t == T_LONG) type = P_LONG;
  if (t == T_VOID) type = P_VOID;
  if (type == P_NONE)
    fatals("Illegal type, token", tokenname(t));

  while (1) {
    scan(&Token);
    if (Token.token != T_STAR) {
      break;
    }
    type = pointer_to(type);
  }

  return (type);
}

/**
    variable_declaration: type identifier ';'
      | type identifier '[' P_INTLIT ']' ';'
      ;
*/

// Parse the declaration of a scalar variable or an array with a given size.
// The identifier has been scanned & we have the type
void var_declaration(int type, int isLocal, int isParam) {
  int id;
  while (1) {
    // array variable
    if (Token.token == T_LBRACKET) {
      scan(&Token);
      if (Token.token == T_INTLIT) {
        // Add this as a known array and generate its space in assembly.
        // We treat the array as a pointer to its elements' type
        if (isLocal) {
          if (isParam) {
            // FIXME: just make them pointers to their underlying type
            fatal("Array parameters are not yet supported");
          }
          addlocl(Text, pointer_to(type), S_ARRAY, isParam, Token.intvalue);
        } else {
          addglob(Text, pointer_to(type), S_ARRAY, Token.intvalue);
        }
      } else {
        fatal("Missing array size in array variable declaration");
      }
      scan(&Token); // integer literal array size
      match(T_RBRACKET, "]");
    // scalar variable
    } else {
      if (isLocal) {
        if (addlocl(Text, type, S_VARIABLE, isParam, 1) == -1) {
          fatals("Duplicate local variable declaration", Text);
        }
      } else {
        addglob(Text, type, S_VARIABLE, 1);
      }
    }

    if (Token.token == T_SEMI) {
      semi();
      return;
    } else if (Token.token == T_COMMA) {
      scan(&Token);
      ident();
      continue;
    } else {
      fatalv("Unexpected token after variable declaration: %s", tokenname(Token.token));
    }
  }
}

// param_declaration: <null>
//           | variable_declaration
//           | variable_declaration ',' param_declaration
//
// Parse the parameters in parentheses after the function name.
// Add them as symbols to the symbol table and return the number
// of parameters.
static int param_declaration(void) {
  int type;
  int paramcnt=0;

  // Loop until the final right parentheses. Current token starts
  // the loop right after T_LPAREN.
  while (Token.token != T_RPAREN) {
    type = parse_type(Token.token);
    ident();
    var_declaration(type, 1, 1);
    paramcnt++;

    switch (Token.token) {
      case T_COMMA:
        scan(&Token);
        break;
      case T_RPAREN:
        break;
      default:
        fatalv("Unexpected token in parameter list: %s", tokenname(Token.token));
    }
  }
  return (paramcnt);
}

struct ASTnode *function_declaration(int type) {
  struct ASTnode *tree, *finalstmt;
  int nameslot;
  int paramcnt;

  nameslot = addglob(Text, type, S_FUNCTION, 0);
  lparen();
  cgresetlocals();
  paramcnt = param_declaration();
  rparen();
  Symtable[nameslot].size = paramcnt;

  Functionid = nameslot; // set currently parsed/generated function
  tree = compound_statement();
  // If the function type isn't P_VOID, check that
  // the last AST operation in the compound statement
  // was a return statement
  if (type != P_VOID) {
    // Error if no statements in the function
    if (tree == NULL) {
      fatal("No statements in function with non-void type");
    }

    finalstmt = (tree->op == A_GLUE) ? tree->right : tree;
    if (finalstmt == NULL || finalstmt->op != A_RETURN) {
      fatal("No return for function with non-void type");
    }
  }
  return (mkuastunary(A_FUNCTION, type, tree, nameslot));
}

void global_declarations(void) {
  struct ASTnode *tree;
  int type;

  while (1) {
    type = parse_type(Token.token);
    ident();
    if (Token.token == T_LPAREN) {
      // Parse the function declaration and
      // generate the assembly code for it
      tree = function_declaration(type);
      if (O_dumpAST) {
        dumpAST(tree, 0, 0);
        fprintf(stdout, "\n");
      }
      if (!O_parseOnly) {
        genAST(tree, NOREG, 0);
      }
    } else {
      // Parse the global variable declaration
      var_declaration(type, 0, 0);
    }

    if (Token.token == T_EOF)
      break;
  }
}
