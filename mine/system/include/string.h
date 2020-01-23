#ifndef _STRING_H
#define _STRING_H_

#include <stddef.h>

char *strdup(char *s);
char *strchr(const char *s, int c);
char *strrchr(const char *s, int c);
int sprintf(char *str, const char *format);
int snprintf(char *str, size_t size, const char *format);
char *strerror(int errnum);

#endif
