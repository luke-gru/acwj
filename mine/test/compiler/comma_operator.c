#include <stdio.h>

int main() {
  int a;
  a = (1, 2, 3); // a = (1, 2),3
  printf("%d\n", a); // => 3
  a = 1, 2, 3;
  printf("%d\n", a); // => 1 `(a = 1), 2, 3`
  return(0);
}
