#include "defs.h"
#include "decl.h"

const char *typename(int ptype) {
  switch (ptype) {
  case P_CHAR:
    return "char";
  case P_INT:
    return "int";
  case P_LONG:
    return "long";
  case P_CHARPTR:
    return "char*";
  case P_LONGPTR:
    return "long*";
  case P_VOIDPTR:
    return "void*";
  default:
    fatald("Invalid typename", ptype);
  }
  return NULL;
}

int inttype(int ptype) {
  switch (ptype) {
    case P_CHAR:
    case P_INT:
    case P_LONG:
      return 1;
    default:
      return 0;
  }
}

int ptrtype(int ptype) {
  switch (ptype) {
    case P_CHARPTR:
    case P_INTPTR:
    case P_LONGPTR:
    case P_VOIDPTR:
      return 1;
    default:
      return 0;
  }
}

struct ASTnode *modify_type(struct ASTnode *tree, int rtype, int op) {
  int ltype;
  int lsize, rsize;

  ltype = tree->type;

  // Compare scalar int types
  if (inttype(ltype) && inttype(rtype)) {

    // Both types same, nothing to do
    if (ltype == rtype) return (tree);

    // Get the sizes for each type
    lsize = genprimsize(ltype);
    rsize = genprimsize(rtype);

    // Tree's size is too big
    if (lsize > rsize) return (NULL);

    // Widen to the right
    if (rsize > lsize) return (mkuastunary(A_WIDEN, rtype, tree, 0));
  }

  // For pointers on the left
  if (ptrtype(ltype)) {
    // OK is same type on right and not doing a binary op
    if (op == 0 && ltype == rtype) return (tree);
  }

  // We can scale only on A_ADD or A_SUBTRACT operation
  if (op == A_ADD || op == A_SUBTRACT) {

    // Left is int type, right is pointer type and the size
    // of the original type is >1: scale the left
    if (inttype(ltype) && ptrtype(rtype)) {
      rsize = genprimsize(value_at(rtype));
      if (rsize > 1)
        return (mkuastunary(A_SCALE, rtype, tree, rsize));
    }
  }

  // If we get here, the types are not compatible
  return (NULL);
}

int pointer_to(int type) {
  int newtype;
  switch (type) {
    case P_VOID: newtype = P_VOIDPTR; break;
    case P_CHAR: newtype = P_CHARPTR; break;
    case P_INT:  newtype = P_INTPTR;  break;
    case P_LONG: newtype = P_LONGPTR; break;
    default:
      fatald("Unrecognised in pointer_to: type", type);
  }
  return (newtype);
}

int value_at(int type) {
  int newtype;
  switch (type) {
    case P_VOIDPTR: newtype = P_VOID; break;
    case P_CHARPTR: newtype = P_CHAR; break;
    case P_INTPTR:  newtype = P_INT;  break;
    case P_LONGPTR: newtype = P_LONG; break;
    default:
      fatald("Unrecognised in value_at: type", type);
  }
  return (newtype);
}
