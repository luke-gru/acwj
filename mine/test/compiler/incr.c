#include <stdio.h>

int list[]= {3, 5, 7, 9, 11, 13, 15};
int *lptr;

int main() {
  int *local_lptr;
  lptr= list;
  printf("%d\n", *lptr); // 3
  lptr= lptr + 1;
  printf("%d\n", *lptr); // 5
  lptr += 1;
  printf("%d\n", *lptr); // 7
  lptr++;
  printf("%d\n", *lptr); // 9
  local_lptr = lptr;
  local_lptr++;
  printf("%d\n", *local_lptr); // 11
  return (0);
}
