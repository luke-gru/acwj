#ifndef _EXECINFO_H
#define _EXECINFO_H

int backtrace(void **buffer, int size);
char **backtrace_symbols(void **buffer, int size);
void backtrace_symbols_fd(void **buffer, int size, int fd);

#endif
