#include <stdio.h>

int main(int argc, char **argv) {
  int a = 0;
  int b = 10;
  int c = 20;
  int d;
  d = a || b || c;
  printf("d: %d\n", d); // 1
  d = a || b || a;
  printf("d: %d\n", d); // 1
  d = b || a || c;
  printf("d: %d\n", d); // 1
  d = a || a || a;
  printf("d: %d\n", d); // 0
  d = a || a;
  printf("d: %d\n", d); // 0
  d = b || a;
  printf("d: %d\n", d); // 1
  d = b || b;
  printf("d: %d\n", d); // 1
  d = b || a || a;
  printf("d: %d\n", d); // 1
  return(0);
}
