#include <stdio.h>
char *ptr = 0;

int main() {
  ptr = NULL;
  ptr = (char*)1;
  printf("ptr NULL: %d\n", ptr);
  printf("ptr 1: %d\n", ptr);
  return (0);
}
