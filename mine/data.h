#ifndef my_cc_data_h
#define my_cc_data_h

// for FILE
#include <stdio.h>

#ifndef EXTERN
#define EXTERN extern
#endif

#define LONGTEXTLEN 1024

// fwd
struct symtable;
struct ASTnode;

// Global variables
// Copyright (c) 2019 Warren Toomey, GPL3

EXTERN int      Line;
EXTERN int      Col;
EXTERN int      Putback;
EXTERN FILE     *Infile;                       // Currently worked on C source code FILE (NOTE: opened by popen(), so don't call fclose() on it)
EXTERN FILE     *Outfile;                      // Currently worked on assembly FILE
EXTERN char     *Infilename;                   // Currently worked on C source code filename
EXTERN char     *Outfilename;                  // Currently worked on assembly filename
EXTERN struct   token Token;
EXTERN char     Text[LONGTEXTLEN + 1];		// Last identifier scanned
EXTERN char     OldText[LONGTEXTLEN + 1];		// Last identifier scanned
EXTERN char     *CurLine;                      // Current line being tokenized

EXTERN struct symtable *CurFunctionSym;        // Symbol ptr of the current function being parsed and code-generated
EXTERN int CommaAsSeparator;                   // flag used in parser to differentiate comma as operator versus comma as separator

// Symbol table lists
EXTERN struct symtable *Globalshead, *Globalstail;	// Global variables and functions
EXTERN struct symtable *Localshead,  *Localstail;	// Local variables
EXTERN struct symtable *Paramshead,  *Paramstail;	// Local parameters
EXTERN struct symtable *Structshead, *Structstail;	// Temp list of struct/union members
EXTERN struct symtable *Membershead, *Memberstail;	// List of struct types
EXTERN struct symtable *Unionshead,  *Unionstail;	// List of union types
EXTERN struct symtable *Enumshead,   *Enumstail;	// List of enum types and values
EXTERN struct symtable *Typeshead,   *Typestail;	// List of typedefs
EXTERN struct symtable *Labelshead,  *Labelstail;	// List of labels for current function, used by gotos

EXTERN int    O_dumpAST;          // option to print AST string representation to stdout
EXTERN int    O_dumpsym;          // option to print symbol table after compiling every file
EXTERN int    O_parseOnly;        // option to not output any assembly language to a file
EXTERN int    O_debugNoisy;       // Turns on various debug messages, which go to stderr
EXTERN int    O_verbose;          // Verbose output for compilation steps only
EXTERN int    O_assemble;         // Do we assemble the assembly files to produce object files
EXTERN int    O_dolink;           // Do we link the object files to produce a binary
EXTERN int    O_keepasm;          // Do we keep the generated assembly file(s)
EXTERN int    O_debugsymbols;     // Do we output debug symbols in the binary
EXTERN int    O_nospill;          // If nospill, exit instead of spilling registers if compiler runs out

EXTERN struct ASTnode *GenNode;   // Currently generated node

#endif
