#ifndef my_cc_data_h
#define my_cc_data_h

// Global variables
// Copyright (c) 2019 Warren Toomey, GPL3

int      Line;
int      Putback;
FILE     *Infile;
FILE     *Outfile;
struct token Token;
char   Text[TEXTLEN + 1];		// Last identifier scanned
struct symtable Gsym[NSYMBOLS];	// Global symbol table
int    Functionid;         // Symbol id of the current function being parsed and code-generated

#endif
