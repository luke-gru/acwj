#include <stdio.h>
#include <stdarg.h>

va_list ap;

void printints(int numints, ...) {
  int val;
  int i;
  va_start(ap, numints);
  for (i = numints; i > 0; i--) {
    val = va_arg(ap, int);
    printint(val);
  }
  va_end(ap);
}

int main() {
  printints(3, 1, 2, 3);
  printints(3, 4, 5, 6);
  return(0);
}
