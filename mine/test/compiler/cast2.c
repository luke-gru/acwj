#include <stdio.h>
char *str= (void *)0;

int main() {
  printf("%s\n", str); // (null)
  str = "a real string!";
  printf("%s\n", str);
  return (0);
}
