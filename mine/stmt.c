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
static int Switchlevel;

struct ASTnode *single_statement(void);

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
  trueAST = single_statement();

  // If we have an 'else', skip it
  // and get the AST for the compound statement
  if (Token.token == T_ELSE) {
    scan(&Token);
    falseAST = single_statement();
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
  bodyAST = single_statement();
  Looplevel--;

  return (mkastnode(A_WHILE, P_NONE, condAST, NULL, bodyAST, NULL, 0));
}


// Parse a FOR statement
// and return its AST
struct ASTnode *for_statement(void) {
  struct ASTnode *condAST, *bodyAST;
  struct ASTnode *preopAST, *postopAST;
  struct ASTnode *tree;

  // Ensure we have 'for' '('
  match(T_FOR, "for");
  lparen();

  // Get the pre_op expr list and the ';'
  preopAST = expression_list(T_SEMI);
  semi();

  // Get the condition and the ';'
  condAST = binexpr(0);
  if (condAST->op < A_EQ || condAST->op > A_GE) {
    condAST = mkuastunary(A_TOBOOL, condAST->type, condAST, NULL, 0);
  }
  semi();

  // Get the post_op statement and the ')'
  postopAST = expression_list(T_RPAREN);
  rparen();

  Looplevel++;
  // Get the compound statement which is the body
  bodyAST = single_statement();
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

struct ASTnode *switch_statement(void) {
  struct ASTnode *left, *n;
  int casecount = 0;
  int inloop = 1;
  int seendefault = 0;
  int casevalue = 0;
  struct ASTnode *casetree = NULL, *casetail = NULL, *c;
  int ASTop;

  match(T_SWITCH, "switch");
  lparen();

  left = binexpr(0);
  rparen();
  lbrace();

  if (!inttype(left->type))
    fatal("Switch expression is not of integer type");

  // Build an A_SWITCH subtree with the expression as
  // the child
  n = mkuastunary(A_SWITCH, 0, left, NULL, 0);
  Switchlevel++;
  while (inloop) {
    switch(Token.token) {
      case T_RBRACE:
        if (casecount == 0) {
          fatal("no cases in switch");
        }
        inloop = 0;
        break;
      case T_CASE:
      case T_DEFAULT:
        if (seendefault) {
          fatal("case or default after existing default");
        }
        if (Token.token == T_DEFAULT) {
          ASTop = A_DEFAULT;
          seendefault = 1;
          scan(&Token);
        } else {
          ASTop = A_CASE;
          scan(&Token);
          left = binexpr(0);
          if (left->op != A_INTLIT) {
            fatal("Expecting integer literal for case value");
          }
          casevalue = left->intvalue;
          // Walk the list of existing case values to ensure
          // that there isn't a duplicate case value
          for (c = casetree; c != NULL; c = c->right) {
            if (casevalue == c->intvalue) {
              fatalv("Duplicate case value %d", c->intvalue);
            }
          }
        }
        match(T_COLON, ":");
        scan_if_match(T_LBRACE);
        left = compound_statement(1);
        scan_if_match(T_RBRACE);
        casecount++;
        // Build a sub-tree with the compound statement as the left child
        // and link it in to the growing A_CASE tree
        if (casetree==NULL) {
          casetree = casetail = mkuastunary(ASTop, 0, left, NULL, casevalue);
        } else {
          casetail->right= mkuastunary(ASTop, 0, left, NULL, casevalue);
          casetail = casetail->right;
        }
        break;
      default:
        fatalv("Unexpected token in switch: %s", tokenname(Token.token));
    }
  }
  Switchlevel--;
  n->intvalue = casecount;
  n->right = casetree;
  rbrace();
  return(n);
}

struct ASTnode *break_statement() {
  if (Looplevel == 0 && Switchlevel == 0)
    fatal("no loop or switch to break out from");
  match(T_BREAK, "break");
  semi();
  return (mkastleaf(A_BREAK, 0, NULL, 0));
}

struct ASTnode *continue_statement() {
  if (Looplevel == 0)
    fatal("no loop to continue to");
  match(T_CONTINUE, "continue");
  semi();
  return (mkastleaf(A_CONTINUE, 0, NULL, 0));
}

struct ASTnode *return_statement(void);

// Parse a single statement
// and return its AST
struct ASTnode *single_statement(void) {
  struct symtable *ctype;
  struct ASTnode *stmt;

  switch (Token.token) {
    case T_LBRACE:
      lbrace();
      stmt = compound_statement(0);
      rbrace();
      return (stmt);
    case T_IDENT:
      // We have to see if the identifier matches a typedef.
      // If not, treat it as an expression.
      // Otherwise, fall down to the declaration_list() call.
      if (type_of_typedef_nofail(Text, &ctype) == -1) {
        stmt = binexpr(0);
        semi();
        return (stmt);
      }
      // fallthru
    case T_CHAR:
    case T_INT:
    case T_LONG:
    case T_EXTERN:
    case T_STRUCT:
    case T_UNION:
    case T_ENUM:
    case T_TYPEDEF:
      declaration_list(&ctype, C_LOCAL, T_SEMI, T_EOF);
      semi();
      return (NULL);
    case T_IF:
      return (if_statement());
    case T_WHILE:
      return (while_statement());
    case T_FOR:
      return (for_statement());
    case T_SWITCH:
      return (switch_statement());
    case T_BREAK:
      return (break_statement());
    case T_CONTINUE:
      return (continue_statement());
    case T_RETURN:
      return (return_statement());
    default:
      stmt = binexpr(0);
      semi(); return (stmt);
  }
}

// Parse a compound statement and return its AST. Last token was '{'
struct ASTnode *compound_statement(int inswitch) {
  struct ASTnode *left = NULL;
  struct ASTnode *tree;

  while (1) {
    // Parse a single statement
    tree = single_statement();

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

    if (Token.token == T_RBRACE) {
      return (left);
    }
    if (inswitch && (Token.token == T_CASE || Token.token == T_DEFAULT)) return (left);
  }
  ASSERT(0);
}

// Parse a return statement and return its AST
struct ASTnode *return_statement(void) {
  struct ASTnode *tree = NULL;
  int returntype, functype;

  ASSERT(CurFunctionSym != NULL); // TODO: error out, must be in function
  // Can't return a value if function returns P_VOID
  if (CurFunctionSym->type == P_VOID)
    fatal("Can't return from a void function");

  // Ensure we have 'return' '('
  match(T_RETURN, "return");
  lparen(); // for some reason, force '(' for now

  if (Token.token != T_RPAREN) {
    // Parse the following expression
    tree = binexpr(0);

    tree = modify_type(tree, CurFunctionSym->type, 0);
    if (tree == NULL)
      fatal("Incompatible types from return"); // TODO: better message

    // Add on the A_RETURN node
    tree = mkuastunary(A_RETURN, P_NONE, tree, NULL, 0);
  }

  rparen();
  semi();
  return (tree);
}
