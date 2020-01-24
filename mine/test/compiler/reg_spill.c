#include <stdio.h>

int add(int x, int y) {
  return(x+y);
}

int main() {
  int result;
  result= 3 * add(2,3) - 5 * add(4,6); // 3 * 5 - 5 * 10 = 15 - 50 = -35
  printf("%d\n", result);
  int x= 5 || 6 && 7 | 8 & 9 << 2 + 3 * 4;
  printf("%d\n", x);
  return(0);
}
