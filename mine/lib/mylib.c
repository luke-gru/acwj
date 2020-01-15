#include <stdio.h>
#include <unistd.h>
#include <errno.h>
void printint(long x) {
  printf("%ld\n", x);
}

void printchar(long x) {
  putc((char)(x & 0x7f), stdout);
}

void printstring(char *str) {
  char *p = str;
  while (*p) {
    printchar(*p);
    p++;
  }
}
