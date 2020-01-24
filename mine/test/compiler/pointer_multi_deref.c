#include <stdio.h>
struct S {
  int val;
};

struct S glob_s;

void decl_list(struct S **sptr) {
  (*sptr)->val = 10;
}

int main() {
  struct S *s;
  s = &glob_s;
  decl_list(&s);
  printf("val: %d\n", s->val);
  return (0);
}
