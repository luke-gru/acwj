#include <stdio.h>
int a;
struct List {
  int val1;
  int val2;
  struct List *next;
};

typedef struct List List;

int main() {
  printf("int size: %d\n", sizeof(int));
  printf("struct List size: %d\n", sizeof(struct List));
  printf("typedef List size: %d\n", sizeof(List));
  printf("struct List* size: %d\n", sizeof(struct List*));
  printf("typedef List* size: %d\n", sizeof(List*));
  return (0);
}
