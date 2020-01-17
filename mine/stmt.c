#include <assert.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

// Parsing of statements
// Copyright (c) 2019 Warren Toomey, GPL3

// compound_statement:          // empty, i.e. no statement
//      |      statement
//      |      statement statements
//      ;
//
// statement: declaration
//      |     if_statement
//      |     while_statement
//      ;
// declaration: 'int' identifier ';'  ;
//
// if_statement: if_head
//      |        if_head 'else' compound_statement
//      ;
//
// while_statement: while '(' true_false_expression ')' compound_statement ;
//
// if_head: 'if' '(' true_false_expression ')' compound_statement  ;
//
// identifier: T_IDENT ;

static int Looplevel;

// Parse an IF statement including
// any optional ELSE clause
// and return its AST
struct ASTnode *if_statement(void) {
  struct ASTnode *condAST, *trueAST, *falseAST = NULL;

  // Ensure we have 'if' '('
  match(T_IF, "if");
  lparen();

  // Parse the following expression
  // and the ')' following. Ensure
  // the tree's operation is a comparison.
  condAST = binexpr(0);

  if (condAST->op < A_EQ || condAST->op > A_GE) {
    condAST = mkuastunary(A_TOBOOL, condAST->type, condAST, NULL, 0);
  }
  rparen();

  // Get the AST for the compound statement
  trueAST = compound_statement();

  // If we have an 'else', skip it
  // and get the AST for the compound statement
  if (Token.token == T_ELSE) {
    scan(&Token);
    falseAST = compound_statement();
  }
  // Build and return the AST for this statement
  return (mkastnode(A_IF, P_NONE, condAST, trueAST, falseAST, NULL, 0));
}

struct ASTnode *while_statement(void) {
  struct ASTnode *condAST, *bodyAST;

  match(T_WHILE, "while");
  lparen();

  condAST = binexpr(0);
  if (condAST->op < A_EQ || condAST->op > A_GE) {
    condAST = mkuastunary(A_TOBOOL, condAST->type, condAST, NULL, 0);
  }
  rparen();

  Looplevel++;
  bodyAST = compound_statement();
  Looplevel--;

  return (mkastnode(A_WHILE, P_NONE, condAST, NULL, bodyAST, NULL, 0));
}

static struct ASTnode *single_statement(void);

// Parse a FOR statement
// and return its AST
static struct ASTnode *for_statement(void) {
  struct ASTnode *condAST, *bodyAST;
  struct ASTnode *preopAST, *postopAST;
  struct ASTnode *tree;

  // Ensure we have 'for' '('
  match(T_FOR, "for");
  lparen();

  // Get the pre_op statement and the ';'
  preopAST = single_statement();
  semi();

  // Get the condition and the ';'
  condAST = binexpr(0);
  if (condAST->op < A_EQ || condAST->op > A_GE) {
    condAST = mkuastunary(A_TOBOOL, condAST->type, condAST, NULL, 0);
  }
  semi();

  // Get the post_op statement and the ')'
  postopAST = single_statement();
  rparen();

  Looplevel++;
  // Get the compound statement which is the body
  bodyAST = compound_statement();
  Looplevel--;

  // For now, all four sub-trees have to be non-NULL.
  // Later on, we'll change the semantics for when some are missing

  // Glue the compound statement and the postop tree
  tree = mkastnode(A_GLUE, P_NONE, bodyAST, NULL, postopAST, NULL, 0);

  // Make a WHILE loop with the condition and this new body
  tree = mkastnode(A_WHILE, P_NONE, condAST, NULL, tree, NULL, 0);

  // And glue the preop tree to the A_WHILE tree
  return (mkastnode(A_GLUE, P_NONE, preopAST, NULL, tree, NULL, 0));
}

static struct ASTnode *break_statement() {
  if (Looplevel == 0)
    fatal("no loop to break out from");
  match(T_BREAK, "break");
  return (mkastleaf(A_BREAK, 0, NULL, 0));
}

static struct ASTnode *continue_statement() {
  if (Looplevel == 0)
    fatal("no loop to continue to");
  match(T_CONTINUE, "continue");
  return (mkastleaf(A_CONTINUE, 0, NULL, 0));
}

static struct ASTnode *return_statement(void);

// Parse a single statement
// and return its AST
static struct ASTnode *single_statement(void) {
  int type;
  int basetype;
  int storage_class = C_LOCAL;
  struct symtable *ctype;
  switch (Token.token) {
    case T_INT:
    case T_CHAR:
    case T_LONG:
    case T_EXTERN:
      basetype = parse_base_type(Token.token, &ctype, &storage_class);
found_type:
      type = parse_pointer_array_type(basetype);
      ident();
      var_declaration(type, ctype, storage_class);
      if (Token.token == T_COMMA) {
        scan(&Token);
        goto found_type;
      } else {
        semi();
      }
      return (NULL);		// No AST generated here
    case T_IF:
      return (if_statement());
    case T_WHILE:
      return (while_statement());
    case T_FOR:
      return (for_statement());
    case T_BREAK:
      return (break_statement());
    case T_CONTINUE:
      return (continue_statement());
    case T_RETURN:
      return (return_statement());
    case T_IDENT:
      if ((basetype = type_of_typedef_nofail(Text, &ctype)) != -1) {
        goto found_type;
      }
      // fallthru
    default:
      return (binexpr(0));
  }
}

// Parse a compound statement
// and return its AST
struct ASTnode *compound_statement(void) {
  struct ASTnode *left = NULL;
  struct ASTnode *tree;

  // Require a left curly bracket
  lbrace();

  while (1) {
    // Parse a single statement
    tree = single_statement();

    // Some statements must be followed by a semicolon
    if (tree != NULL &&
       (tree->op == A_ASSIGN || tree->op == A_RETURN || tree->op == A_FUNCALL ||
        tree->op == A_BREAK || tree->op == A_CONTINUE)) {
      semi();
    }

    // For each new tree, either save it in left
    // if left is empty, or glue the left and the
    // new tree together
    if (tree != NULL) {
      if (left == NULL) {
        left = tree;
      } else {
        left = mkastnode(A_GLUE, P_NONE, left, NULL, tree, NULL, 0);
      }
    }
    // When we hit a right curly bracket,
    // skip past it and return the AST
    if (Token.token == T_RBRACE) {
      rbrace();
      return (left);
    }
  }
  assert(0);
}

// Parse a return statement and return its AST
static struct ASTnode *return_statement(void) {
  struct ASTnode *tree;
  int returntype, functype;

  assert(CurFunctionSym != NULL); // TODO: error out, must be in function
  // Can't return a value if function returns P_VOID
  if (CurFunctionSym->type == P_VOID)
    fatal("Can't return from a void function");

  // Ensure we have 'return' '('
  match(T_RETURN, "return");
  lparen();

  // Parse the following expression
  tree = binexpr(0);

  tree = modify_type(tree, CurFunctionSym->type, 0);
  if (tree == NULL)
    fatal("Incompatible types from return"); // TODO: better message

  // Add on the A_RETURN node
  tree = mkuastunary(A_RETURN, P_NONE, tree, NULL, 0);

  rparen();
  return (tree);
}
