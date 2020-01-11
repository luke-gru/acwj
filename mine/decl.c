#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of declarations
// Copyright (c) 2019 Warren Toomey, GPL3

// Parse the current token and
// return a primitive type enum value
int parse_type(int t) {
  if (t == T_CHAR) return (P_CHAR);
  if (t == T_INT)  return (P_INT);
  if (t == T_LONG) return (P_LONG);
  if (t == T_VOID) return (P_VOID);
  fatals("Illegal type, token", tokenname(t));
}

// Parse the declaration of a variable
void var_declaration(void) {
  int id, type;

  type = parse_type(Token.token);
  scan(&Token);
  ident();
  id = addglob(Text, type, S_VARIABLE);
  genglobsym(id);
  semi();
}

struct ASTnode *function_declaration(void) {
  int type;
  struct ASTnode *tree, *finalstmt;
  int nameslot;

  type = parse_type(Token.token);
  scan(&Token);
  ident();
  nameslot = addglob(Text, type, S_FUNCTION);
  Functionid = nameslot; // set currently parsed/generated function
  lparen();
  rparen();

  tree = compound_statement();
  // If the function type isn't P_VOID, check that
  // the last AST operation in the compound statement
  // was a return statement
  if (type != P_VOID) {
    finalstmt = (tree->op == A_GLUE) ? tree->right : tree;
    if (finalstmt == NULL || finalstmt->op != A_RETURN)
      fatal("No return for function with non-void type");
  }
  return (mkuastunary(A_FUNCTION, type, tree, nameslot));
}
