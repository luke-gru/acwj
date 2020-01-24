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
    condAST = mkastunary(A_TOBOOL, condAST->type, condAST->ctype, condAST, NULL, 0);
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
  return (mkastnode(A_IF, P_NONE, NULL, condAST, trueAST, falseAST, NULL, 0));
}

struct ASTnode *while_statement(void) {
  struct ASTnode *condAST, *bodyAST;

  match(T_WHILE, "while");
  lparen();

  condAST = binexpr(0);
  if (condAST->op < A_EQ || condAST->op > A_GE) {
    condAST = mkastunary(A_TOBOOL, condAST->type, condAST->ctype, condAST, NULL, 0);
  }
  rparen();

  Looplevel++;
  bodyAST = single_statement();
  Looplevel--;

  return (mkastnode(A_WHILE, P_NONE, NULL, condAST, NULL, bodyAST, NULL, 0));
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
    condAST = mkastunary(A_TOBOOL, condAST->type, condAST->ctype, condAST, NULL, 0);
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
  tree = mkastnode(A_GLUE, P_NONE, NULL, bodyAST, NULL, postopAST, NULL, 0);

  // Make a WHILE loop with the condition and this new body
  tree = mkastnode(A_WHILE, P_NONE, NULL, condAST, NULL, tree, NULL, 0);

  // And glue the preop tree to the A_WHILE tree
  return (mkastnode(A_GLUE, P_NONE, NULL, preopAST, NULL, tree, NULL, 0));
}

struct ASTnode *switch_statement(void) {
  struct ASTnode *left, *n;
  int casecount = 0;
  int inloop = 1;
  int seendefault = 0;
  int casevalue = 0;
  int lbracematched = 0;
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
  n = mkastunary(A_SWITCH, P_NONE, NULL, left, NULL, 0);
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
        // FIXME: allow default as first case
        if (seendefault) {
          fatal("case or default after existing default");
        }
        if (Token.token == T_DEFAULT) {
          ASTop = A_DEFAULT;
          seendefault = 1;
          scan(&Token); // default
          casevalue = 0;
          debugnoisy("parse", "switch: found default case");
        } else {
          ASTop = A_CASE;
          scan(&Token); // case
          left = binexpr(0);
          if (left->op != A_INTLIT) {
            fatal("Expecting integer literal for case value");
          }
          casevalue = left->intvalue;
          debugnoisy("parse", "switch: found case: %d", casevalue);
          // Walk the list of existing case values to ensure
          // that there isn't a duplicate case value
          for (c = casetree; c != NULL; c = c->right) {
            if (casevalue == c->intvalue && c->op != A_DEFAULT) {
              fatalv("Duplicate case value %d", c->intvalue);
            }
          }
        }
        match(T_COLON, ":");
        casecount++;
        // case: 1 case 2:
        if (Token.token == T_CASE) {
          left = NULL;
          debugnoisy("parse", "switch: found fallthru case: %d", casevalue);
        } else {
          lbracematched = scan_if_match(T_LBRACE);
          left = compound_statement(1);
          if (lbracematched)
            scan_if_match(T_RBRACE);
          debugnoisy("parse", "switch: found non-empty case: %d", casevalue);
        }
        // Build a sub-tree with the compound statement as the left child
        // and link it in to the growing A_CASE tree
        if (casetree==NULL) {
          casetree = casetail = mkastunary(ASTop, P_NONE, NULL, left, NULL, casevalue);
        } else {
          casetail->right = mkastunary(ASTop, P_NONE, NULL, left, NULL, casevalue);
          casetail = casetail->right;
        }
        break;
      default:
        ASSERT(0);
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
  return (mkastleaf(A_BREAK, P_NONE, NULL, NULL, 0));
}

struct ASTnode *continue_statement() {
  if (Looplevel == 0)
    fatal("no loop to continue to");
  match(T_CONTINUE, "continue");
  semi();
  return (mkastleaf(A_CONTINUE, P_NONE, NULL, NULL, 0));
}

struct ASTnode *return_statement(void);
struct ASTnode *goto_statement(void);
struct ASTnode *label_statement(void);
struct ASTnode *empty_statement(void);

// Parse a single statement
// and return its AST
struct ASTnode *single_statement(void) {
  struct symtable *ctype;
  struct ASTnode *stmt = NULL;

  switch (Token.token) {
    case T_LBRACE:
      lbrace();
      if (Token.token == T_RBRACE) {
        rbrace();
        return (empty_statement());
      }
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
      declaration_list(&ctype, C_LOCAL, T_SEMI, T_EOF, &stmt);
      semi();
      return (stmt); // can be NULL
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
    case T_GOTO:
      return (goto_statement());
    case T_LABEL:
      return (label_statement());
    default:
      stmt = binexpr(0);
      semi(); return (stmt);
  }
  ASSERT(0);
  return (NULL);
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
        left = mkastnode(A_GLUE, P_NONE, NULL, left, NULL, tree, NULL, 0);
      }
    }

    if (Token.token == T_RBRACE) {
      return (left);
    }
    if (inswitch && (Token.token == T_CASE || Token.token == T_DEFAULT)) return (left);
  }
  ASSERT(0);
  return (NULL);
}

// Parse a return statement and return its AST
struct ASTnode *return_statement(void) {
  struct ASTnode *tree = NULL;
  int returntype, functype;

  ASSERT(CurFunctionSym != NULL);

  // Ensure we have 'return' '('
  match(T_RETURN, "return");
  if (Token.token == T_SEMI) {
    if (CurFunctionSym->type != P_VOID) {
      fatal("Incompatible types from return: can't return void from non-void function");
    }
    tree = mkastunary(A_RETURN, P_VOID, NULL, NULL, NULL, 0);
    semi();
    return (tree);
  }
  lparen(); // for some reason, force '(' for now

  if (Token.token != T_RPAREN) {
    // Parse the following expression
    tree = binexpr(0);

    tree = modify_type(tree, CurFunctionSym->type, CurFunctionSym->ctype, A_RETURN);
    if (tree == NULL)
      fatal("Incompatible types from return"); // TODO: better message

    // Add on the A_RETURN node
    tree = mkastunary(A_RETURN, tree->type, tree->ctype, tree, NULL, 0);
  }

  rparen();
  semi();
  return (tree);
}

struct ASTnode *goto_statement(void) {
  ASSERT(CurFunctionSym != NULL);
  struct symtable *labelsym;
  struct ASTnode *n;
  scan(&Token); // goto
  ident(); // label name
  labelsym = add_or_find_label(Text);
  n = mkastunary(A_GOTO, P_NONE, NULL, NULL, labelsym, 0);
  semi();
  return (n);
}

struct ASTnode *label_statement(void) {
  ASSERT(CurFunctionSym != NULL);
  struct symtable *labelsym;
  struct ASTnode *n;
  labelsym = add_or_find_label(Text);
  n = mkastunary(A_LABEL, P_NONE, NULL, NULL, labelsym, 0);
  scan(&Token); // the T_LABEL, ':' included (Text is filled with name)
  return (n);
}

struct ASTnode *empty_statement(void) {
  struct ASTnode *n;
  n = mkastunary(A_EMPTY, P_NONE, NULL, NULL, NULL, 0);
  return (n);
}
