#include "defs.h"
#define extern_
#include "data.h"
#undef extern_
#include "decl.h"
#include <errno.h>

// Compiler setup and top-level execution
// Copyright (c) 2019 Warren Toomey, GPL3
//
#ifndef INCDIR
#error "INCDIR must be defined"
#endif
#define CPPCMD "cpp -nostdinc -isystem "

// Initialise global variables
static void init() {
  Line = 1;
  Putback = '\n';
  CurFunctionSym = NULL;
  addglob("printint", P_VOID, NULL, S_FUNCTION, C_GLOBAL, 1);
  addglob("printchar", P_VOID, NULL, S_FUNCTION, C_GLOBAL, 1);
  addglob("printstring", P_VOID, NULL, S_FUNCTION, C_GLOBAL, 1);
  O_dumpAST = 0;
  O_parseOnly = 1;
  Outfile = stdout; // just in case we try to dump some assembly somewhere, that this isn't NULL and doesn't segfault
  setup_signal_handlers();
}

// Print out a usage if started incorrectly
static void usage(char *prog) {
  fprintf(stderr, "Usage: %s infile\n", prog);
  exit(1);
}

// Main program: check arguments and print a usage
// if we don't have an argument. Open up the input
// file and call scanfile() to scan the tokens in it.
void main(int argc, char *argv[]) {
  char cmd[TEXTLEN];
  char *filename;
  if (argc < 2)
    usage(argv[0]);

  init();

  // Scan for command-line options
  for (int i=1; i<argc; i++) {
    if (*argv[i] != '-') break;
    for (int j=1; argv[i][j]; j++) {
      switch (argv[i][j]) {
        case 'T':
          O_dumpAST = 1;
          break;
        default:
          fprintf(stderr, "Invalid option: %c\n", argv[i][j]);
          usage(argv[0]);
      }
    }
  }

  filename = argv[argc-1];
  snprintf(cmd, TEXTLEN, "%s %s %s", CPPCMD, INCDIR, filename);
  if ((Infile = popen(cmd, "r")) == NULL) {
    fatalv("Unable to open %s: %s\n", filename, strerror(errno));
  }
  Infilename = strdup(filename);

  scan(&Token);			// Get the first token from the input
  global_declarations();
  exit(0);
}
