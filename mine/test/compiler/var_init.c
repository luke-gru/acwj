#include <stdio.h>

int f;
int g = 1;
int a[10];                                      // Ten zeroed elements
char b[]= { 'q', 'w', 'e', 'r', 't', 'y' };     // Six elements
char c[10]= { 'q', 'w', 'e', 'r', 't', 'y' };   // Ten elements, zero padded
char *hi = "hello";

int main() {
  printf("f: %d\n", f);
  printf("g: %d\n", g);
  printf("a[0]: %d\n", a[0]);
  printf("b[0]: %c\n", b[0]);
  printf("c[2]: %c\n", c[2]);
  printf("%s\n", hi);
  return (0);
}
