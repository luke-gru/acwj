#include <assert.h>
#include "defs.h"
#include "data.h"
#include "decl.h"

static struct token *Rejtoken = NULL;

char *toknames[] = {
  "T_EOF",

  "T_ASSIGN", "T_LOGOR", "T_LOGAND",
  "T_BITOR", "T_BITXOR", "T_AMPER",
  "T_EQ", "T_NE",
  "T_LT", "T_GT", "T_LE", "T_GE",
  "T_LSHIFT", "T_RSHIFT",
  "T_PLUS", "T_MINUS", "T_STAR", "T_SLASH",

  "T_INC", "T_DEC", "T_INVERT", "T_LOGNOT",

  "T_INTLIT", "T_SEMI", "T_IDENT", "T_STRLIT",
  "T_LBRACE", "T_RBRACE", "T_LPAREN", "T_RPAREN", "T_LBRACKET", "T_RBRACKET",
  "T_COMMA", "T_DOT", "T_ARROW",
  // keywords
  "T_INT", "T_IF", "T_ELSE", "T_WHILE", "T_FOR", "T_BREAK", "T_CONTINUE",
  "T_VOID", "T_CHAR", "T_LONG", "T_STRUCT", "T_UNION", "T_ENUM",
  "T_TYPEDEF", "T_RETURN", "T_EXTERN",
  NULL
};

char *tokenname(int tok) {
  assert(tok >= T_EOF && tok < T_LAST);
  return toknames[tok];
}


// Lexical scanning
// Copyright (c) 2019 Warren Toomey, GPL3

// Return the position of character c
// in string s, or -1 if c not found
static int chrpos(char *s, int c) {
  char *p;

  p = strchr(s, c);
  return (p ? p - s : -1);
}

// Get the next character from the input file.
static int next(void) {
  int c;
  int l;

  if (Putback) {		// Use the character put
    c = Putback;		// back if there is one
    Putback = 0;
    Col++;
    return (c);
  }

  c = fgetc(Infile);		// Read from input file

  while (c == '#') {                    // We've hit a pre-processor statement
    scan(&Token);                       // Get the line number into l
    if (Token.token != T_INTLIT)
      fatals("Expecting pre-processor line number, got:", Text);
    l = Token.intvalue;

    scan(&Token);                       // Get the filename in Text
    if (Token.token != T_STRLIT)
      fatals("Expecting pre-processor file name, got:", Text);

    if (Text[0] != '<') {               // If this is a real filename
      if (strcmp(Text, Infilename) != 0)     // and not the one we have now
        Infilename = strdup(Text);      // save it. Then update the line num
      Line = l;
      Col = 0;
    }

    // Skip to the end of the line
    while ((c = fgetc(Infile)) != '\n') {
    }
    Col = 0;
    c = fgetc(Infile);                  // and get the next character
  }

  Col++;
  if ('\n' == c) {
    Line++;			// Increment line count
    Col = 0;
  }
  return (c);
}

// Put back an unwanted character
static void putback(int c) {
  assert(!Putback);
  Putback = c;
  Col--;
}

// similar to putback(), but API function for parser
void reject_token(struct token *t) {
  if (Rejtoken) {
    fatal("Can't reject token twice");
  }
  Rejtoken = t;
}

// Skip past input that we don't need to deal with,
// i.e. whitespace, newlines. Return the first
// character we do need to deal with.
static int skip(void) {
  int c;

  c = next();
  while (' ' == c || '\t' == c || '\n' == c || '\r' == c || '\f' == c) {
    c = next();
  }
  return (c);
}

static int skip_until(int until_char) {
  int c;
  while ((c = next()) != until_char) {
    if (c == EOF) return EOF;
  }
  return (c);
}

// Scan and return an integer literal
// value from the input file. Store
// the value as a string in Text.
static int scanint(int c) {
  int k, val = 0;

  // Convert each character into an int value
  while ((k = chrpos("0123456789", c)) >= 0) {
    val = val * 10 + k;
    c = next();
  }

  // We hit a non-integer character, put it back.
  putback(c);
  return (val);
}

// Scan an identifier from the input file and
// store it in buf[]. Return the identifier's length
static int scanident(int c, char *buf, int lim) {
  int i = 0;

  // Allow digits, alpha and underscores
  while (isalpha(c) || isdigit(c) || '_' == c) {
    // Error if we hit the identifier length limit,
    // else append to buf[] and get next character
    if (lim - 1 == i) {
      fatal("Identifier too long");
    } else if (i < lim - 1) {
      buf[i++] = c;
    }
    c = next();
  }
  // We hit a non-valid character, put it back.
  // NUL-terminate the buf[] and return the length
  putback(c);
  buf[i] = '\0';
  return (i);
}

// Given a word from the input, return the matching
// keyword token number or 0 if it's not a keyword.
// Switch on the first letter so that we don't have
// to waste time strcmp()ing against all the keywords.
static int keyword(char *s) {
  switch (*s) {
    case 'e':
      if (!strcmp(s, "else"))
        return (T_ELSE);
      if (!strcmp(s, "enum"))
        return (T_ENUM);
      if (!strcmp(s, "extern"))
        return (T_EXTERN);
      break;
    case 'i':
      if (!strcmp(s, "if"))
        return (T_IF);
      if (!strcmp(s, "int"))
        return (T_INT);
      break;
    case 'c':
      if (!strcmp(s, "char"))
        return (T_CHAR);
      if (!strcmp(s, "continue"))
        return (T_CONTINUE);
      break;
    case 'w':
      if (!strcmp(s, "while"))
        return (T_WHILE);
      break;
    case 'f':
      if (!strcmp(s, "for"))
        return (T_FOR);
      break;
    case 'b':
      if (!strcmp(s, "break"))
        return (T_BREAK);
      break;
    case 'v':
      if (!strcmp(s, "void"))
        return (T_VOID);
      break;
    case 'l':
      if (!strcmp(s, "long"))
        return (T_LONG);
      break;
    case 'r':
      if (!strcmp(s, "return"))
        return (T_RETURN);
      break;
    case 's':
      if (!strcmp(s, "struct"))
        return (T_STRUCT);
      break;
    case 'u':
      if (!strcmp(s, "union"))
        return (T_UNION);
    case 't':
      if (!strcmp(s, "typedef"))
        return (T_TYPEDEF);

  }
  return (0);
}

int scan_ch(void) {
  int c;

  // Get the next input character and interpret
  // metacharacters that start with a backslash
  c = next();
  if (c == '\\') {
    switch (c = next()) {
      case 'a':  return '\a';
      case 'b':  return '\b';
      case 'f':  return '\f';
      case 'n':  return '\n';
      case 'r':  return '\r';
      case 't':  return '\t';
      case 'v':  return '\v';
      case '\\': return '\\';
      case '"':  return '"' ;
      case '\'': return '\'';
      default:
        fatalc("unknown escape sequence", c);
    }
  }
  return (c); // Just an ordinary old character!
}

// Scan in a string literal from the input file,
// and store it in buf[]. Return the length of
// the string.
int scan_str(char *buf) {
  int i, c;

  // Loop while we have enough buffer space
  for (i=0; i<TEXTLEN-1; i++) {
    // Get the next char and append to buf
    // Return when we hit the ending double quote
    if ((c = scan_ch()) == '"') {
      buf[i] = '\0';
      return (i);
    }
    buf[i] = c;
  }
  // Ran out of buf[] space
  fatal("String literal too long");
  return (0);
}

// Scan and return the next token found in the input.
// Return 1 if token valid, 0 if no tokens left.
int scan(struct token *t) {
  int c, tokentype;

  if (Rejtoken) {
    t = Rejtoken;
    Rejtoken = NULL;
    return (1);
  }

gettok:
  // Skip whitespace
  c = skip();

  // Determine the token based on
  // the input character
  switch (c) {
    case EOF:
      t->token = T_EOF;
      return (0);
    case '+':
      if ((c = next()) == '+') {
        t->token = T_INC;
      } else {
        putback(c);
        t->token = T_PLUS;
      }
      break;
    case '-':
      if ((c = next()) == '-') {
        t->token = T_DEC;
      } else if (c == '>') {
        t->token = T_ARROW;
      } else {
        putback(c);
        t->token = T_MINUS;
      }
      break;
    case '*':
      t->token = T_STAR;
      break;
    case '/':
      if ((c = next()) == '/') {
        if (skip_until('\n') == EOF) {
          return EOF;
        } else {
          goto gettok;
        }
      } else if (c == '*') {
        int until;
        while ((until = skip_until('*')) != EOF) {
          if (next() == '/') {
            goto gettok;
          }
        }
        return EOF;
      } else {
        putback(c);
      }
      t->token = T_SLASH;
      break;
    case ';':
      t->token = T_SEMI;
      break;
    case '{':
      t->token = T_LBRACE;
      break;
    case '}':
      t->token = T_RBRACE;
      break;
    case '(':
      t->token = T_LPAREN;
      break;
    case ')':
      t->token = T_RPAREN;
      break;
    case '[':
      t->token = T_LBRACKET;
      break;
    case ']':
      t->token = T_RBRACKET;
      break;
    case '=':
      if ((c = next()) == '=') {
        t->token = T_EQ;
      } else {
        putback(c);
        t->token = T_ASSIGN;
      }
      break;
    case '!':
      if ((c = next()) == '=') {
        t->token = T_NE;
      } else {
        putback(c);
        t->token = T_LOGNOT;
      }
      break;
    case '<':
      if ((c = next()) == '=') {
        t->token = T_LE;
      } else if (c == '<') {
        t->token = T_LSHIFT;
      } else {
        putback(c);
        t->token = T_LT;
      }
      break;
    case '>':
      if ((c = next()) == '=') {
        t->token = T_GE;
      } else if (c == '>') {
        t->token = T_RSHIFT;
      } else {
        putback(c);
        t->token = T_GT;
      }
      break;
    case '&':
      if ((c = next()) == '&') {
        t->token = T_LOGAND;
      } else {
        putback(c);
        t->token = T_AMPER;
      }
      break;
    case '|':
      if ((c = next()) == '|') {
        t->token = T_LOGOR;
      } else {
        putback(c);
        t->token = T_BITOR;
      }
      break;
    case '^':
      t->token = T_BITXOR;
      break;
    case '~':
      t->token = T_INVERT;
      break;
    case ',':
      t->token = T_COMMA;
      break;
    case '\'':
      t->intvalue = scan_ch();
      t->token = T_INTLIT;
      if (next() != '\'') {
        fatal("Expected ' at end of char literal");
      }
      break;
    case '"':
      scan_str(Text);
      t->token = T_STRLIT;
      break;
    case '.':
      t->token = T_DOT;
      break;
    default:
      // If it's a digit, scan the
      // literal integer value in
      if (isdigit(c)) {
        t->intvalue = scanint(c);
        t->token = T_INTLIT;
        break;
      } else if (isalpha(c) || '_' == c) {
        // Read in a keyword or identifier
        scanident(c, Text, TEXTLEN);

        // If it's a recognised keyword, return that token
        if (tokentype = keyword(Text)) {
          t->token = tokentype;
          break;
        }
        // Not a recognised keyword, so it must be an identifier
        t->token = T_IDENT;
        break;
      }
      // The character isn't part of any recognised token, error
      fatalc("Unrecognised character", c);
  }

  // We found a token
  return (1);
}
