#include <stdio.h>

int main() {
  int x = 0;

  switch (x) {
    case 0:
    case 1:
    case 2:
      printf("non-default: %d\n", x);
      break;
    default:
      printf("default: %d\n", x);
      break;
  }
  return (0);
}
