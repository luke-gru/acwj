#include <stdio.h>

int main() {
  int a = 0;
  int i;
  for (i = 0; i < 100; i++) {
    if (i == 10) {
      goto done;
    }
  }
done:
  printf("i: %d\n", i);
  return(0);
}
