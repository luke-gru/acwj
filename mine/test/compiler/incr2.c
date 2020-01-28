#include <stdio.h>

struct Val {
  char *val;
};

struct Val v;

int main() {
  int a = 1;
  int b;
  char *ptr;
  v.val = "hello";
  ptr = v.val++;

  printf("v.val: %s\n", v.val);
  printf("ptr: %s\n", ptr);
  printf("++ptr: %s\n", ++ptr);
  printf("++v.val: %s\n", ++v.val);
  printf("--v.val: %s\n", --v.val);
  printf("v.val--: %s\n", v.val--);
  printf("v.val: %s\n", v.val);
  return (0);
}

