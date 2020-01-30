#include <stdio.h>

int main(void) {
  int a, b, c, d;
  int e;
  a = 10;
  b = 5;
  c = 2;
  d = 1;
  e = (a > b) ? (a+b) : a;
  printf("e: %d\n", e); // 15
  printf("d: %d\n", d); // 1
  d += ((a > b) ? (a+b) : a);
  printf("a: %d\n", a); // 10
  printf("b: %d\n", b); // 5
  printf("d: %d\n", d); // 16
  d = 2;
  d *= ((a > b) ? (a+b) : a);
  printf("d: %d\n", d); // 30
  return(0);
}
