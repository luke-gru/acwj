#include <stdio.h>
int   d, f;
int  *e;

int main() {
  int a, b, c;
  b= 3;
  c= 5;
  a= b + c * 10;
  printf("%d\n", a);

  d= 12;
  printf("%d\n", d);
  e= &d;
  f= *e;
  printf("%d\n", f);
  return(0);
}
