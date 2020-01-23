#include <stdio.h>

int a[10];
int *b;

int main(int argc, char **argv) {
  a[1] = 1;
  a[2] = 2;
  b = a; // TODO: allow &a[0] too
  printf("%d\n", *(b+0));
  printf("%d\n", b[0]);
  printf("%d\n", *(b+1));
  printf("%d\n", b[1]);
  printf("%d\n", *(b+2));
  printf("%d\n", b[2]);
  return(0);
}
