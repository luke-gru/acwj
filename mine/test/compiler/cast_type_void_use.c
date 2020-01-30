#include <stdio.h>

int main(int argc, char *argv[]) {
  int a;
  (void)0;
  a = (int)(void)5; // should disallow this
  printf("%d\n", a);
  return(0);
}
