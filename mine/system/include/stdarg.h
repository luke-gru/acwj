#ifndef _STDARG_H_
#define _STDARG_H_

typedef struct Va_List {
    int gp_offset;
    int fp_offset;
    long overflow_arg_area;
    char *reg_save_area;
} va_list;

#define va_start(list, param) \
    list.gp_offset = 50;\
    list.fp_offset = 0;\
    list.overflow_arg_area = (long)__builtin_vararg_addr_setup();\
    list.reg_save_area = 0

#define va_arg(list, type) ((type)(list.overflow_arg_area = list.overflow_arg_area + 8), (*(type*)(list.overflow_arg_area - 8)))
#define va_end(list)

#endif
