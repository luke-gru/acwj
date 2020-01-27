#ifndef _STDLIB_H
#define _STDLIB_H_

#include <stddef.h>

void exit(int status);
int system(const char *command);
void *malloc(size_t size);
void free(void *ptr);
void *calloc(size_t nmemb, size_t size);
void *realloc(void *ptr, size_t size);
int abs(int j);

#endif
