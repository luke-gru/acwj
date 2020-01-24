#include <stdio.h>

int main(int argc, char **argv) {
  int a = 0;
  int b = 10;
  int c = 20;
  int d;
  d = a && b && c;
  printf("d: %d\n", d); // 0
  d = b && c;
  printf("d: %d\n", d); // 1
  return(0);
}
