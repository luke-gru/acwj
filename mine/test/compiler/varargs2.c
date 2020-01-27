#include <stdarg.h>
#include <stdio.h>

void fatalv(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stdout, fmt, ap);
  va_end(ap);
}

int main() {
  fatalv("hey there %s: %d, %d, %c\n", "string", 6, 8, 'c');
  return (0);
}

