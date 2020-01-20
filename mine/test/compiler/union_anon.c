#include <stdio.h>
struct S {
  union {
    int a;
    int b;
  };
  int c;
};

struct S s;

int main() {
  s.a = 10;
  s.b = 20;
  s.c = 40;
  printf("s.a: %d\n", s.a); // 20
  printf("s.b: %d\n", s.b); // 20
  printf("s.c: %d\n", s.c); // 40

  return (0);
}
