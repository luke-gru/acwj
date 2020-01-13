#ifndef my_cc_data_h
#define my_cc_data_h

// Global variables
// Copyright (c) 2019 Warren Toomey, GPL3

int      Line;
int      Putback;
FILE     *Infile;
FILE     *Outfile;
char     *Infilename;
struct token Token;
char   Text[TEXTLEN + 1];		// Last identifier scanned
#define Gsym Symtable
struct symtable Symtable[NSYMBOLS];	// Global symbol table
int    Globs;              // Position of next free global slot
int    Locls;              // Position of next free local slot
int    Functionid;         // Symbol id of the current function being parsed and code-generated
int    O_dumpAST;          // option to print AST string representation to stdout
int    O_parseOnly;        // option to not output any assembly language to a file

#endif
