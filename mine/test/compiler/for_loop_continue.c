#include <stdio.h>
int main() {
  int i;
  for (i = 0; i < 10; i++) {
    if (i == 5) {
      continue;
    } else {
      printf("i: %d\n", i);
    }
  }
  printf("i: %d\n", i);
  return (0);
}
