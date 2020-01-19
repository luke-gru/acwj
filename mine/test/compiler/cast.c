#include <stdio.h>
int main() {
  int   x= 65535;
  char  y= (char)x;     // y is now 255, the lower 8 bits
  printf("y: %d\n", y);
  return (0);
}
