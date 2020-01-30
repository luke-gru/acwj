#ifndef _STRING_H
#define _STRING_H_

#include <stddef.h>

char *strdup(char *s);
char *strchr(const char *s, int c);
char *strrchr(const char *s, int c);
int sprintf(char *str, const char *format);
int snprintf(char *str, size_t size, const char *format);
char *strerror(int errnum);
int strcmp(const char *s1, const char *s2);
void *memcpy(void *dest, const void *src, size_t n);
size_t strlen(const char *s);
char *strcat(char *dest, const char *src);
char *strstr(const char *haystack, const char *needle);
char *strsignal(int sig);

#endif
