#include <stdio.h>
struct Foo {
  union {
    int a;
    int b;
  } v;
  int z;
};

struct Foo f;

int main() {
  printf("f.v.a: %d, f.v.b: %d\n", f.z.a, f.z.b);
  return (0);
}
