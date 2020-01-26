#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
  int argc = 4;
  int i;
  int err;
  err = close(100);
  if (err != 0) {
    fprintf(stdout, "Error: %s\n", strerror(errno));
  }
  return (0);
}


