#include <stdio.h>

struct S {
  int val;
};

void assign_val_copy_struct(struct S s, int val) {
  s.val = val;
}

void assign_val_take_struct(struct S *s, int val) {
  s->val = val;
}

int main() {
  struct S s;
  s.val = 10;
  printf("s.val: %d\n", s.val); // 10
  assign_val_copy_struct(s, 20);
  printf("s.val: %d\n", s.val); // 10
  assign_val_take_struct(&s, 30);
  printf("s.val: %d\n", s.val); // 30
  return (0);
}
