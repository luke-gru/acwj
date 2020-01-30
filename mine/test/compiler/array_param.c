#include <stdio.h>
char *gstr = "string";

int main(int argc, char *argv[]) {
  printf("%s\n", argv[0]);
  argv = &gstr; // should work, `argv` is not actually an array, it's a variable
  printf("%s\n", argv[0]);
  return(0);
}
