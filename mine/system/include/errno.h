#ifndef _ERRNO_H_
# define _ERRNO_H_

extern int __errno_location();
#define errno __errno_location()

#endif
