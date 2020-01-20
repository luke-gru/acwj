#include <stdio.h>
struct Bar {
  int b;
};
struct Foo {
  union {
    int a;
    int b;
  } v;
  struct Bar b;
  int z;
};

struct Foo f;

int main() {
  f.v.a = 10;
  f.b.b = 20;
  f.z = 30;
  printf("f.v.a: %d\n", f.v.a);
  printf("f.b.b: %d\n", f.b.b);
  printf("f.z: %d\n", f.z);
  return (0);
}
