#include <stdarg.h>
#include <stdio.h>

int main() {
  int a = 2;
  // FIXME: this fails:
  // int b = (a += 2, a)
  int b;
  b = (a += 2, a);
  printf("a: %d, b: %d\n", a, b);
  return (0);
}

