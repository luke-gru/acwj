#include "defs.h"
#include "data.h"
#include "decl.h"

static struct token *Rejtoken = NULL;
static int CurLinePos = 0;
static size_t CurLineSize = 0;
static size_t CurLineLen = 0;
static int IsEOF = 0;
static int InPreprocLine = 0;
static int AtBol = 1; // beginning of line
static int FirstNonSpaceInLine = 0; // used to track AtBol

#ifdef DEBUG_SCANNER
#define scandebug(...) debugnoisy("scan", __VA_ARGS__)
#else
#define scandebug(...)
#endif

char *toknames[] = {
  "T_EOF",
  "T_COMMA", "T_ASSIGN",
  "T_AS_PLUS", "T_AS_MINUS", "T_AS_STAR", "T_AS_SLASH",
  "T_QUESTION",
  "T_LOGOR", "T_LOGAND",
  "T_BITOR", "T_BITXOR", "T_AMPER",
  "T_EQ", "T_NE",
  "T_LT", "T_GT", "T_LE", "T_GE",
  "T_LSHIFT", "T_RSHIFT",
  "T_PLUS", "T_MINUS", "T_STAR", "T_SLASH", "T_PERCENT",

  "T_INC", "T_DEC", "T_INVERT", "T_LOGNOT",

  "T_INTLIT", "T_SEMI", "T_IDENT", "T_STRLIT",
  "T_LBRACE", "T_RBRACE", "T_LPAREN", "T_RPAREN", "T_LBRACKET", "T_RBRACKET",
  "T_DOT", "T_ARROW", "T_COLON",
  // keywords
  "T_INT", "T_IF", "T_ELSE", "T_WHILE", "T_FOR", "T_BREAK", "T_CONTINUE",
  "T_VOID", "T_CHAR", "T_LONG", "T_STRUCT", "T_UNION", "T_ENUM",
  "T_SWITCH", "T_CASE", "T_DEFAULT", "T_TYPEDEF", "T_RETURN", "T_EXTERN",
  "T_SIZEOF", "T_STATIC", "T_CONST", "T_GOTO", "T_LABEL",
  NULL
};
// must line up with tokens enum to compile
CASSERT(sizeof(toknames)/sizeof(char*) == (T_LAST+1))

char *tokenname(int tok) {
  ASSERT(tok >= T_EOF && tok < T_LAST);
  return (toknames[tok]);
}

// Lexical scanning
// Copyright (c) 2019 Warren Toomey, GPL3

// Return the position of character c
// in string s, or -1 if c not found
static long chrpos(char *s, int c) {
  char *p;

  p = strchr(s, c);
  return (p ? (p - s) : -1);
}

void reset_scanner(void) {
  Rejtoken = NULL;
  CurLinePos = 0;
  CurLineSize = 0;
  CurLineLen = 0;
  IsEOF = 0;
  InPreprocLine = 0;
  AtBol = 1; // beginning of line
  FirstNonSpaceInLine = 0; // used to track AtBol
  Line = 1;
  Col = 0;
  Putback = '\n';
}

static int readchar(void) {
  ssize_t res = 0;
  if (IsEOF) {
    return (EOF);
  }
  if (CurLinePos == CurLineLen) { // new line
    CurLinePos = 0;
    AtBol = 1;
    res = (ssize_t)getline(&CurLine, &CurLineSize, Infile);
    if (res == -1) {
      IsEOF = 1;
      return (EOF);
    }
    scandebug("getline() got '%s'", CurLine);
    CurLineLen = res;
  }
  if (!CurLine) { // first line
    CurLinePos = 0;
    AtBol = 1;
    res = (ssize_t)getline(&CurLine, &CurLineSize, Infile);
    if (res == -1) {
      IsEOF = 1;
      return (EOF);
    }
    scandebug("getline() got '%s'", CurLine);
    CurLineLen = res;
  }
  int c = CurLine[CurLinePos++];
  if (CurLinePos == 1 || (AtBol && isspace(c))) {
    AtBol = 1;
    FirstNonSpaceInLine = 0;
  } else if (AtBol && !FirstNonSpaceInLine) {
    AtBol = 1;
    FirstNonSpaceInLine = 1;
  } else {
    AtBol = 0;
    FirstNonSpaceInLine = 0;
  }
  return (c);
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

  c = readchar();

  int preproc_comment = 0;
  int aminpreproc = InPreprocLine;

  if (!InPreprocLine && CurLinePos == 1 && c == '#') {
    aminpreproc = InPreprocLine = 1;
    preproc_comment = 1;
    memcpy(OldText, Text, LONGTEXTLEN+1);
  }
  while (CurLinePos == 1 && c == '#') {                    // We've hit a pre-processor statement
    scandebug("skipping preproc line %s", CurLine);
    scan(&Token);                       // Get the line number into l
    if (Token.token != T_INTLIT) {
      fatals("Expecting pre-processor line number, got:", Text);
    }
    l = Token.intvalue;

    scan(&Token);                       // Get the filename in Text
    if (Token.token != T_STRLIT) {
      fatalv("Expecting pre-processor file name, got: %s", tokenname(Token.token));
    }

    if (Text[0] != '<') {               // If this is a real filename
      if (strcmp(Text, Infilename) != 0) {     // and not the one we have now
        Infilename = strdup(Text);      // save it. Then update the line num
      }
      Line = l;
    }

    // Skip to the end of the line
    while ((c = readchar()) != '\n') {
      scandebug("next skip EOL: %c", c);
      if (c == EOF) {
        return (EOF);
      }
    }
    Col = 0;
    c = readchar();                  // and get the next character
  }
  if (preproc_comment) {
    memcpy(Text, OldText, LONGTEXTLEN+1);
  }

  Col++;
  if ('\n' == c) {
    Line++;			// Increment line count
    Col = 0;
  }
  if (aminpreproc && Col == 0) {
    InPreprocLine = 0;
  }
  scandebug("%snext(): %c (%d)", InPreprocLine ? "PRE " : "", c, c);
  return (c);
}

// Put back an unwanted character
static void putback(int c) {
  ASSERT(!Putback);
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
    if (c == EOF) return (EOF);
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
  long val = 0, radix = 10;
  long k;

  if (c == '0') {
    if ((c = next()) == 'x') {
      radix = 16;
      c = next();
    } else {
      radix = 8;
    }
  }

  // Convert each character into an int value
  while ((k = chrpos("0123456789abcdef", tolower(c))) >= 0) {
    if (k >= radix) {
      fatalc("Invalid digit in integer literal", c);
    }
    val = val * radix + k;
    c = next();
  }

#define MY_MAX_INT (0x7FFFFFFF)
  if (val > MY_MAX_INT) {
    fatalv("Value %ld is out of range of integer literal", val);
  }

  // We hit a non-integer character, put it back.
  putback(c);
  return ((int)val);
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
      buf[i++] = (char)c;
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
      if (!strcmp(s, "case"))
        return (T_CASE);
      if (!strcmp(s, "const"))
        return (T_CONST);
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
    case 'd':
      if (!strcmp(s, "default"))
        return (T_DEFAULT);
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
      if (!strcmp(s, "switch"))
        return (T_SWITCH);
      if (!strcmp(s, "sizeof"))
        return (T_SIZEOF);
      if (!strcmp(s, "static"))
        return (T_STATIC);
      break;
    case 'u':
      if (!strcmp(s, "union"))
        return (T_UNION);
      break;
    case 't':
      if (!strcmp(s, "typedef"))
        return (T_TYPEDEF);
      break;
    case 'g':
      if (!strcmp(s, "goto"))
        return (T_GOTO);
      break;
    default:
      break;

  }
  return (0);
}

// Read in a hexadecimal constant from the input
static int hexchar(void) {
  long c, h, n = 0, f = 0;

  // Loop getting characters
  while (isxdigit(c = next())) {
    // Convert from char to int value
    h = chrpos("0123456789abcdef", tolower(c));
    // Add to running hex value
    n = n * 16 + h;
    f = 1;
  }
  // We hit a non-hex character, put it back
  putback(c);
  // Flag tells us we never saw any hex characters
  if (!f)
    fatal("missing digits after '\\x'");
  if (n > 255)
    fatal("value out of range after '\\x'");
  return ((int)n);
}

int scan_ch(void) {
  int i, c, c2;

  // Get the next input character and interpret
  // metacharacters that start with a backslash
  c = next();
  if (c == '\\') {
    switch (c = next()) {
      case 'a':  return ('\a');
      case 'b':  return ('\b');
      case 'f':  return ('\f');
      case 'n':  return ('\n');
      case 'r':  return ('\r');
      case 't':  return ('\t');
      case 'v':  return ('\v');
      case '\\': return ('\\');
      case '"':  return ('"' );
      case '\'': return ('\'');
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
        for (i = c2 = 0; isdigit(c) && c < '8'; c = next()) {
          if (++i > 3)
            break;
          c2 = c2 * 8 + (c - '0');
        }
        putback(c);             // Put back the first non-octal char
        return (c2);
      case 'x':
        return (hexchar());
      default:
        fatalc("unknown escape sequence", c);
    }
  }
  return (c); // Just an ordinary old character!
}

// Scan in a string literal from the input file,
// and store it in buf[]. Return the length of
// the string.
int scan_str(char *buf, int start) {
  int i, c;

  // Loop while we have enough buffer space
  for (i=start; i<LONGTEXTLEN - 1; i++) {
    // Get the next char and append to buf
    // Return when we hit the ending double quote
    if ((c = scan_ch()) == '"') {
      buf[i] = '\0';
      return (i);
    }
    buf[i] = (char)c;
  }
  // Ran out of buf[] space
  fatalv("String literal too long, max length is %d bytes", LONGTEXTLEN);
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
      } else if (c == '=') {
        t->token = T_AS_PLUS;
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
      } else if (c == '=') {
        t->token = T_AS_MINUS;
      } else if (isdigit(c)) {
        t->intvalue = -(scanint(c));
        t->token = T_INTLIT;
      } else {
        putback(c);
        t->token = T_MINUS;
      }
      break;
    case '*':
      if ((c = next()) == '=') {
        t->token = T_AS_STAR;
      } else {
        putback(c);
        t->token = T_STAR;
      }
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
      } else if (c == '=') {
        t->token = T_AS_SLASH;
        break;
      } else {
        putback(c);
      }
      t->token = T_SLASH;
      break;
    case '%':
      t->token = T_PERCENT;
      break;
    case ';':
      t->token = T_SEMI;
      break;
    case ':':
      t->token = T_COLON;
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
      scan_str(Text, 0);
      t->token = T_STRLIT;
      if (!InPreprocLine) {
        while ((c = skip()) == '"') {
          scan_str(Text, strlen(Text));
        }
        putback(c);
      }
      break;
    case '.':
      t->token = T_DOT;
      break;
    case '?':
      t->token = T_QUESTION;
      break;
    default:
      // If it's a digit, scan the
      // literal integer value in
      if (isdigit(c)) {
        t->intvalue = scanint(c);
        t->token = T_INTLIT;
        break;
      } else if (isalpha(c) || '_' == c) {
        int atbol = AtBol;
        // Read in a keyword or identifier
        scanident(c, Text, LONGTEXTLEN);

        // If it's a recognised keyword, return that token
        if ((tokentype = keyword(Text))) {
          t->token = tokentype;
          break;
        }
        // Not a recognised keyword, so it must be an identifier
        t->token = T_IDENT;
        c = 0;
        if (atbol && (c = next()) == ':') {
          t->token = T_LABEL;
        } else {
          if (c) putback(c);
        }
        break;
      }
      // The character isn't part of any recognised token, error
      fatalv("Unrecognised character '%c' (%d)", c, (int)c);
  }

  // We found a token
  return (1);
}
