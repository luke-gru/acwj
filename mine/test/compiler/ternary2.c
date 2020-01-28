#include <stdio.h>
#include <string.h>

static int chrpos(char *s, int c) {
  char *p;

  p = strchr(s, c);
  return (p ? p - s : -1);
}

char *str = "porkpie hat";

int main() {
  int pos;
  pos = chrpos(str, 'i');
  printf("pos: %d\n", pos);
  pos = chrpos(str, 'w');
  printf("pos: %d\n", pos);
  return (0);
}

