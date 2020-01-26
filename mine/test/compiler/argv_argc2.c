#include <stdio.h>

char *argv[] = {
  "",
  "-v",
  "-t",
  "file.c"
};

int main() {
  int argc = 4;
  int i;
  // Scan for command-line options
  for (i = 1; i<argc; i++) {
    fprintf(stdout, "Processing arg %s\n", argv[i]);
    // No leading '-', stop scanning for options
    if (*argv[i] != '-') break;
    fprintf(stdout, "Processing option %s\n", argv[i]);
  }
  fprintf(stdout, "Got file %s\n", argv[i]);
  return (0);
}


