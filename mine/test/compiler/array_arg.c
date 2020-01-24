#include <stdio.h>
int a[10] = {1,2,3,4,5};

void printary(int *a, int size) {
  int i;
  for(i = 0; i < size; i++) {
    printf("%d: %d\n", i, a[i]);
  }
}

int main() {
  printary(a, 10);
  return (0);
}
