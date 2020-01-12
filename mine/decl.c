#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of declarations
// Copyright (c) 2019 Warren Toomey, GPL3

// Parse the current token and
// return a primitive type enum value
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
void var_declaration(int type) {
  int id;
  while (1) {
    if (Token.token == T_LBRACKET) {
      scan(&Token);
      if (Token.token == T_INTLIT) {
        // Add this as a known array and generate its space in assembly.
        // We treat the array as a pointer to its elements' type
        id = addglob(Text, pointer_to(type), S_ARRAY, Token.intvalue);
        genglobsym(id);
      } else {
        fatal("Missing array size in array variable declaration");
      }
      scan(&Token); // integer literal array size
      match(T_RBRACKET, "]");
    } else {
      id = addglob(Text, type, S_VARIABLE, 1);
      genglobsym(id);
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

struct ASTnode *function_declaration(int type) {
  struct ASTnode *tree, *finalstmt;
  int nameslot;

  nameslot = addglob(Text, type, S_FUNCTION, 0);
  Functionid = nameslot; // set currently parsed/generated function
  lparen();
  rparen();

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
      var_declaration(type);
    }

    if (Token.token == T_EOF)
      break;
  }
}
