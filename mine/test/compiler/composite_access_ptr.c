#include <stdio.h>
struct Bar {
    int val;
};
struct Foo {
    int val;
    struct Bar *bar;
};

struct Foo foo;
struct Bar bar;
struct Foo *fooptr;
int ival;

int main() {
  ival = 10;
  fooptr = &foo;
  fooptr->bar = &bar;
  fooptr->bar->val = ival;
  printf("fooptr->bar->val == %d\n", fooptr->bar->val);
  return(0);
}
