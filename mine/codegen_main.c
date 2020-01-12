#include "defs.h"
#include "data.h"
#include "decl.h"
#include <errno.h>

// Compiler setup and top-level execution
// Copyright (c) 2019 Warren Toomey, GPL3

// Initialise global variables
static void init() {
  Line = 1;
  Putback = '\n';
  Functionid = -1;
  addglob("printint", P_VOID, S_FUNCTION, 0);
  addglob("printchar", P_VOID, S_FUNCTION, 0);
  O_dumpAST = 0;
  O_parseOnly = 0;
}

// Print out a usage if started incorrectly
static void usage(char *prog) {
  fprintf(stderr, "Usage: %s [-T] infile\n", prog);
  exit(1);
}

// Main program: check arguments and print a usage
// if we don't have an argument. Open up the input
// file and call scanfile() to scan the tokens in it.
void main(int argc, char *argv[]) {
  struct ASTnode *tree;

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

  // Open up the input file
  if ((Infile = fopen(argv[argc-1], "r")) == NULL) {
    fatalv("Unable to open %s: %s\n", argv[argc-1], strerror(errno));
  }
  Infilename = strdup(argv[argc-1]);

  // Create the output file
  if ((Outfile = fopen("out.s", "w")) == NULL) {
    fclose(Infile);
    fatalv("Unable to create out.s: %s\n", strerror(errno));
  }

  scan(&Token);			// Get the first token from the input

  genpreamble();		// Output the preamble
  global_declarations();
  genpostamble();

  fclose(Infile);
  fclose(Outfile);		// Close the output file and exit
  exit(0);
}
