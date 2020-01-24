#ifndef my_cc_data_h
#define my_cc_data_h

// fwd
struct symtable;
struct ASTnode;

// Global variables
// Copyright (c) 2019 Warren Toomey, GPL3

int      Line;
int      Col;
int      Putback;
FILE     *Infile;                       // Currently worked on C source code FILE (NOTE: opened by popen(), so don't call fclose() on it)
FILE     *Outfile;                      // Currently worked on assembly FILE
char     *Infilename;                   // Currently worked on C source code filename
char     *Outfilename;                  // Currently worked on assembly filename
struct   token Token;
char     Text[TEXTLEN + 1];		// Last identifier scanned
char     OldText[TEXTLEN + 1];		// Last identifier scanned
char     *CurLine;                      // Current line being tokenized

struct symtable *CurFunctionSym;        // Symbol ptr of the current function being parsed and code-generated

// Symbol table lists
struct symtable *Globalshead, *Globalstail;	// Global variables and functions
struct symtable *Localshead,  *Localstail;	// Local variables
struct symtable *Paramshead,  *Paramstail;	// Local parameters
struct symtable *Structshead, *Structstail;	// Temp list of struct/union members
struct symtable *Membershead, *Memberstail;	// List of struct types
struct symtable *Unionshead,  *Unionstail;	// List of union types
struct symtable *Enumshead,   *Enumstail;	// List of enum types and values
struct symtable *Typeshead,   *Typestail;	// List of typedefs
struct symtable *Labelshead,  *Labelstail;	// List of labels for current function, used by gotos

int    O_dumpAST;          // option to print AST string representation to stdout
int    O_dumpsym;          // option to print symbol table after compiling every file
int    O_parseOnly;        // option to not output any assembly language to a file
int    O_debugNoisy;       // Turns on various debug messages, which go to stderr
int    O_verbose;          // Verbose output for compilation steps only
int    O_assemble;         // Do we assemble the assembly files to produce object files
int    O_dolink;           // Do we link the object files to produce a binary
int    O_keepasm;          // Do we keep the generated assembly file(s)

struct ASTnode *GenNode;   // Currently generated node

#endif
