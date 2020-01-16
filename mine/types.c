#include "defs.h"
#include "decl.h"

// NOTE: allocates strings, potentially a lot
char *typename(int ptype) {
  if (ptrtype(ptype)) {
    int valuetype = value_at(ptype);
    char *valuename = typename(valuetype);
    return str_concat(valuename, "*");
  }
  switch (ptype) {
  case P_CHAR:
    return strdup("char");
  case P_INT:
    return strdup("int");
  case P_LONG:
    return strdup("long");
  case P_VOID:
    return strdup("void");
  default:
    fatald("Invalid typename", ptype);
  }
  return NULL;
}

int inttype(int ptype) {
  return ((ptype & P_PTR_BITS) == 0);
}

int ptrtype(int ptype) {
  return ((ptype & P_PTR_BITS) != 0);
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
    if (rsize > lsize) return (mkuastunary(A_WIDEN, rtype, tree, NULL, 0));
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
      if (rsize > 1) {
        return (mkuastunary(A_SCALE, rtype, tree, NULL, rsize));
      } else {
        return (tree); // Size 1, no need to scale
      }
    }
  }

  // If we get here, the types are not compatible
  return (NULL);
}

// Given a primitive type, return
// the type which is a pointer to it
int pointer_to(int type) {
  // too many levels of indirection
  if ((type & P_PTR_BITS) == P_PTR_BITS) {
    fatald("Unrecognised in pointer_to: type", type);
  }
  return (type + 1);
}

int value_at(int type) {
  if ((type & 0xf) == 0x0) {
    fatald("Unrecognised in value_at: type", type);
  }
  return (type - 1);
}

// Given a type and a composite type pointer, return
// the size of this type in bytes
int typesize(int type, struct symtable *ctype) {
  if (type == P_STRUCT)
    return (ctype->size);
  return (genprimsize(type));
}
