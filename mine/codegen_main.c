#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

#define MAXOBJ 100
char *objlist[MAXOBJ];        // List of object file names
int objcnt = 0;               // Position to insert next name

#if COMPILE_ASSERTS
#ifndef INCDIR
#error "INCDIR must be defined"
#endif
#endif
#define CPPCMD "cpp -nostdinc -isystem "

// Compiler setup and top-level execution
// Copyright (c) 2019 Warren Toomey, GPL3

// TODO: Remove this once I change all these calls in test/compiler to use printf() instead
static void init_symtable() {
  addglob("printint", P_VOID, NULL, S_FUNCTION, C_GLOBAL, 1);
  addparam("myint", P_INT, NULL, S_VARIABLE, 1);

  addglob("printchar", P_VOID, NULL, S_FUNCTION, C_GLOBAL, 1);
  addparam("mychar", P_CHAR, NULL, S_VARIABLE, 1);

  addglob("printstring", P_VOID, NULL, S_FUNCTION, C_GLOBAL, 1);
  addparam("mystring", pointer_to(P_CHAR), NULL, S_VARIABLE, 1);

  // compiler builtins
  addglob("__builtin_vararg_addr_setup", P_INT, NULL, S_FUNCTION, C_GLOBAL, 1);

  freeloclsyms();
}

// Initialise global variables
static void init() {
  Line = 1;
  Putback = '\n';
  CurFunctionSym = NULL;

  O_dumpAST = 0; // when set to 1, so is O_parseOnly
  O_parseOnly = 0;
  O_debugNoisy = 0;
  O_verbose = 0;
  O_assemble = 0; // assemble and keep object files
  O_dolink = 0;   // assemble and link
  O_keepasm = 0;

  init_symtable();
  setup_signal_handlers();
}

// Print out a usage if started incorrectly
static void usage(char *prog) {
  fprintf(stderr,
      "Usage: %s [-vcDST] [-o outfile] infile [infile2 ...]\n"
       "       -v give verbose output of the compilation stages\n"
       "       -c generate object files but don't link them\n"
       "       -D print debug info to stderr\n"
       "       -S generate assembly files but don't link them\n"
       "       -T dump the AST trees for each input file\n"
       "       -o outfile, produce the outfile executable file\n",
      prog);
  exit(1);
}

// Given a string with a '.' and at least a 1-character suffix
// after the '.', change the suffix to be the given character.
// Return the new string or NULL if the original string could
// not be modified
char *alter_suffix(char *str, char suffix) {
  char *posn;
  char *newstr;

  // Clone the string
  if ((newstr = strdup(str)) == NULL) return (NULL);

  // Find the '.'
  if ((posn = strrchr(newstr, '.')) == NULL) return (NULL);

  // Ensure there is a suffix
  posn++;
  if (*posn == '\0') return (NULL);

  // Change the suffix and NUL-terminate the string
  *posn++ = suffix; *posn = '\0';
  return (newstr);
}

// Given an input filename, compile that file
// down to assembly code. Return the new file's name
char *do_compile(char *filename) {
  char cmd[TEXTLEN];

  Outfilename = alter_suffix(filename, 's');
  if (Outfilename == NULL) {
    fprintf(stderr, "Error: %s has no suffix, try .c on the end\n", filename);
    exit(1);
  }

  snprintf(cmd, TEXTLEN, "%s %s %s", CPPCMD, INCDIR, filename);

  // Open up the input file
  if ((Infile = popen(cmd, "r")) == NULL) {
    fprintf(stderr, "Unable to open %s: %s\n", filename, strerror(errno));
    exit(1);
  }
  Infilename = filename;
  // Create the output file
  if ((Outfile = fopen(Outfilename, "w")) == NULL) {
    fprintf(stderr, "Unable to create %s: %s\n", Outfilename,
            strerror(errno));
    exit(1);
  }

  Line = 1;                     // Reset the scanner
  Putback = '\n';
  clear_symtable();             // Clear the symbol table
  init_symtable();
  if (O_verbose)
    printf("compiling %s\n", filename);
  scan(&Token);                 // Get the first token from the input
  genpreamble();                // Output the preamble
  global_declarations();        // Parse the global declarations
  genpostamble();               // Output the postamble
  fclose(Outfile);              // Close the output file
  freestaticsyms();
  return (Outfilename);
}

#define ASCMD "as -o "
// Given an input filename, assemble that file
// down to object code. Return the object filename
char *do_assemble(char *filename) {
  char cmd[TEXTLEN];
  int err;

  char *outfilename = alter_suffix(filename, 'o');
  if (outfilename == NULL) {
    fprintf(stderr, "Error: %s has no suffix, try .s on the end\n", filename);
    exit(1);
  }
  // Build the assembly command and run it
  snprintf(cmd, TEXTLEN, "%s %s %s", ASCMD, outfilename, filename);
  if (O_verbose) printf("%s\n", cmd);
  err = system(cmd);
  if (err != 0) { fprintf(stderr, "Assembly of %s failed\n", filename); exit(1); }
  return (outfilename);
}

// FIXME: remove linking to mylib.c
#define LDCMD "cc lib/mylib.c -o"
// Given a list of object files and an output filename,
// link all of the object filenames together.
void do_link(char *outfilename, char *objlist[]) {
  int cnt, size = TEXTLEN;
  char cmd[TEXTLEN], *cptr;
  int err;

  // Start with the linker command and the output file
  cptr = cmd;
  cnt = snprintf(cptr, size, "%s %s ", LDCMD, outfilename);
  cptr += cnt; size -= cnt;

  // Now append each object file
  while (*objlist != NULL) {
    cnt = snprintf(cptr, size, "%s ", *objlist);
    cptr += cnt; size -= cnt; objlist++;
  }

  if (O_verbose) printf("%s\n", cmd);
  err = system(cmd);
  if (err != 0) { fprintf(stderr, "Linking failed\n"); exit(1); }
}

#define AOUT "a.out"

// Main program: check arguments and print a usage
// if we don't have an argument. Open up the input
// file and call scanfile() to scan the tokens in it.
void main(int argc, char *argv[]) {
  struct ASTnode *tree;
  char *asmfile, *objfile;
  char *binname = AOUT;

  if (argc < 2)
    usage(argv[0]);

  init();

  O_dolink = 1; // by default, output a binary a.out

  int i;
  // Scan for command-line options
  for (i = 1; i<argc; i++) {
    // No leading '-', stop scanning for options
    if (*argv[i] != '-') break;
    for (int j=1; *argv[i] == '-' && argv[i][j]; j++) {
      switch (argv[i][j]) {
        case 'T':
          O_dumpAST = 1; O_parseOnly = 1; O_dolink = 0; break;
        case 'D':
          O_debugNoisy = 1; break;
        case 'v':
          O_verbose = 1; break;
        case 'o':
          binname = argv[++i]; O_dolink = 1; break;
        case 'c':
          O_assemble = 1; O_keepasm = 0; O_dolink = 0; break;
        case 'S':
          O_keepasm = 1; O_assemble = 0; O_dolink = 0; break;
        default:
          fprintf(stderr, "Invalid option: %c\n", argv[i][j]);
          usage(argv[0]);
      }
    }
  }

  // Work on each input file in turn
  while (i < argc) {
    asmfile = do_compile(argv[i]);      // Compile the source file

    if (O_dolink || O_assemble) {
      objfile = do_assemble(asmfile);   // Assemble it to object format
      if (objcnt == (MAXOBJ - 2)) {
        fprintf(stderr, "Too many object files for the compiler to handle\n");
        exit(1);
      }
      objlist[objcnt++] = objfile;      // Add the object file's name
      objlist[objcnt] = NULL;           // to the list of object files
    }

    if (!O_keepasm)                     // Remove the assembly file if
      unlink(asmfile);                  // we don't need to keep it
    i++;
  }

  if (O_dolink) {
    do_link(binname, objlist);
    // If we don't need to keep the object
    // files, then remove them
    if (!O_assemble) {
      for (i = 0; objlist[i] != NULL; i++) {
        unlink(objlist[i]);
      }
    }
  }

  exit(0);
}
