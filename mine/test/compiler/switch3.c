#include <stdio.h>

int main() {
  int x = 0;
  int y = 10;

  switch (x) {
    case 0:
    case 1:
    case 2:
      switch (y) {
        case 10:
          printf("breaking from inner with %d\n", y);
          break;
        case 20:
          printf("Error at 20\n");
        default:
          printf("Error!\n");
      }
      printf("non-default: %d\n", x);
      break;
    default:
      printf("default: %d\n", x);
      break;
  }
  printf("finished\n");
  return (0);
}
