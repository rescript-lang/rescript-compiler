#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <libunwind.h>

value ml_func_with_10_params_native(value x1, value x2, value x3, value x4,
                                    value x5, value x6, value x7, value x8,
                                    value x9, value x10) {
    return Val_unit;
}

void error() {
    exit(1);
}

void perform_stack_walk() {
    unw_context_t ctxt;
    unw_getcontext(&ctxt);

    unw_cursor_t cursor;
    {
        int result = unw_init_local(&cursor, &ctxt);
        if (result != 0) error();
    }

    int reached_main = 0;

    for (;;) {
        {
            char procname[256];
            unw_word_t ip_offset; // IP - start_of_proc
            int result = unw_get_proc_name(&cursor, procname, sizeof(procname),
                                           &ip_offset);
            if (result != 0) error();
            if (strcmp(procname, "main") == 0)
                reached_main = 1;
            //printf("%s + %lld\n", procname, (long long int)ip_offset);
        }

        {
            int result = unw_step(&cursor);
            if (result == 0) break;
            if (result < 0) error();
        }
    }

    //printf("Reached end of stack.\n");
    if (!reached_main) {
        //printf("Failure: Did not reach main.\n");
        error();
    }
}

value ml_perform_stack_walk() {
    perform_stack_walk();
    return Val_unit;
}
