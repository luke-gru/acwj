#include "defs.h"
#include "data.h"
#include "decl.h"
#include <errno.h>

#ifndef INCDIR
#define INCDIR "/tmp/include"
#endif
#define CPPCMD "cpp -nostdinc -isystem "

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
  FILE *f = stderr;
  while (scan(&Token)) {
    printf("scanned\n");
    fprintf(f, "Token %s", tokenname(Token.token));
    if (Token.token == T_INTLIT)
      fprintf(f, ", value %d", Token.intvalue);
    if (Token.token == T_IDENT || Token.token == T_STRLIT)
      fprintf(f, ", value '%s'", Text);
    fprintf(f, " (at %s: %d:%d)", Infilename, Line, column());
    fprintf(f, "\n");
  }
}

// Main program: check arguments and print a usage
// if we don't have an argument. Open up the input
// file and call scanfile() to scan the tokens in it.
int main(int argc, char **argv) {
  char cmd[TEXTLEN];
  char *filename;
  int rawread = 0;
  if (argc < 2)
    usage(argv[0]);

  O_debugNoisy=0;

  filename = argv[1];
  if (argc == 3 && !strcmp(argv[2], "-raw")) {
    rawread = 1;
    fprintf(stderr, "(raw read)\n");
  }

  init();

  if (rawread) {
    if ((Infile = fopen(filename, "r")) == NULL) {
      fprintf(stderr, "Unable to open %s: %s\n", filename, strerror(errno));
      exit(1);
    }
  } else {
    snprintf(cmd, TEXTLEN, "%s %s %s", CPPCMD, INCDIR, filename);

    if ((Infile = popen(cmd, "r")) == NULL) {
      fprintf(stderr, "Unable to open %s: %s\n", filename, strerror(errno));
      exit(1);
    }
  }

  Infilename = strdup(filename);

  scanfile();
  exit(0);
  return (0);
}
