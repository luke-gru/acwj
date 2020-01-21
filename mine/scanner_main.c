#include "defs.h"
#include "data.h"
#include "decl.h"
#include <errno.h>

#ifndef INCDIR
#error "INCDIR must be defined"
#endif
#define CPPCMD "cpp -nostdinc -isystem "

// Compiler setup and top-level execution
// Copyright (c) 2019 Warren Toomey, GPL3

// Compiler setup and top-level execution
// Copyright (c) 2019 Warren Toomey, GPL3

// Initialise global variables
static void init() {
  Line = 1;
  Col = 0;
  Putback = '\n';
  setup_signal_handlers();
}

// Print out a usage if started incorrectly
static void usage(char *prog) {
  fprintf(stderr, "Usage: %s infile\n", prog);
  exit(1);
}

// Loop scanning in all the tokens in the input file.
// Print out details of each token found.
static void scanfile() {
  struct token T;

  while (scan(&T)) {
    printf("Token %s", tokenname(T.token));
    if (T.token == T_INTLIT)
      printf(", value %d", T.intvalue);
    if (T.token == T_IDENT)
      printf(", value '%s'", Text);
    printf(" (at %s: %d:%d)", Infilename, Line, column());
    printf("\n");
  }
}

// Main program: check arguments and print a usage
// if we don't have an argument. Open up the input
// file and call scanfile() to scan the tokens in it.
void main(int argc, char *argv[]) {
  char cmd[TEXTLEN];
  char *filename;
  if (argc != 2)
    usage(argv[0]);

  filename = argv[1];

  init();

  snprintf(cmd, TEXTLEN, "%s %s %s", CPPCMD, INCDIR, filename);

  if ((Infile = popen(cmd, "r")) == NULL) {
    fprintf(stderr, "Unable to open %s: %s\n", filename, strerror(errno));
    exit(1);
  }
  Infilename = strdup(filename);

  scanfile();
  exit(0);
}
