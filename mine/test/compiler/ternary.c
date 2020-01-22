#include <stdio.h>

int x;
int y;

int main() {
  for (y= 0; y < 10; y++) {
    x = (y < 4) ? y :
      (y > 7) ? y+1000 : y + 9000;
    printf("%d\n", x);
  }

  return(0);
}
