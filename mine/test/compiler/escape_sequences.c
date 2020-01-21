#include <stdio.h>
char nullbyte = '\0';
char maxbyte = '\xFF';
int maxint = 0x7FFFFFFF;

int main() {
  printf("nullbyte: %d\n", nullbyte);
  printf("maxbyte: %d\n", maxbyte);
  printf("maxint: %d\n", maxint);
  return(0);
}
