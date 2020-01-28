#include <stdarg.h>
#include <stdio.h>

struct Val {
  int val;
};

struct Val v;

int main() {
  int a = 1;
  a += 1;
  v.val = 0;
  v.val += 1;
  printf("a: %d\n", a);
  printf("v.val: %d\n", v.val);
  return (0);
}

