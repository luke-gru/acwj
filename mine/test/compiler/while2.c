#include <stdio.h>
#include <string.h>

char *string = "\t\f\t hello and goodbye for now";
int pos = 0;

int next(void) {
  int res;
  if (pos == strlen(string)) return (EOF);
  res = string[pos];
  pos++;
  return (res);
}

static int skip(void) {
  int c;

  c = next();
  while (' ' == c || '\t' == c || '\n' == c || '\r' == c || '\f' == c) {
    if (c == EOF) return (EOF);
    c = next();
  }
  return (c);
}

int main() {
  int c;
  while ((c = skip()) != EOF) {
    printf("c: %c, pos: %d\n", c, pos);
  }
  return (0);
}

