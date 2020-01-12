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
struct symtable Gsym[NSYMBOLS];	// Global symbol table
int    Functionid;         // Symbol id of the current function being parsed and code-generated
int    O_dumpAST;          // option to print AST string representation to stdout
int    O_parseOnly;        // option to not output any assembly language to a file

#endif
