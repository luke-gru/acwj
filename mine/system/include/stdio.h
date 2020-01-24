#ifndef _STDIO_H_
#define _STDIO_H_

#include <stddef.h>
#include <stdarg.h>

#define EOF (-1)

// This FILE definition will do for now
typedef char *FILE;
extern FILE *stderr;
extern FILE *stdout;

FILE *fopen(char *pathname, char *mode);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
size_t fwrite(void *ptr, size_t size, size_t nmemb, FILE *stream);
int fclose(FILE *stream);
int printf(char *format);
int fprintf(FILE *stream, char *format);
size_t getline(char **lineptr, size_t *n, FILE *stream);

FILE *popen(const char *command, const char *type);
int pclose(FILE *stream);

int vfprintf(FILE *stream, const char *format, va_list ap);

#endif	// _STDIO_H_
