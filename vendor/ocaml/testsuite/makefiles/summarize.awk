#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#         Damien Doligez, projet Gallium, INRIA Rocquencourt            #
#                                                                       #
#   Copyright 2013 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

function check() {
    if (!in_test){
        printf("error at line %d: found test result without test start\n", NR);
        errored = 1;
    }
}

function clear() {
    curfile = "";
    in_test = 0;
}

function record_pass() {
    check();
    ++ passed;
    clear();
}

function record_skip() {
    check();
    ++ skipped;
    clear();
}

function record_fail() {
    check();
    ++ failed;
    fail[failidx++] = sprintf ("%s/%s", curdir, curfile);
    clear();
}

function record_unexp() {
    ++ unexped;
    unexp[unexpidx++] = sprintf ("%s/%s", curdir, curfile);
    clear();
}

/Running tests from '[^']*'/ {
    if (in_test) record_unexp();
    match($0, /Running tests from '[^']*'/);
    curdir = substr($0, RSTART+20, RLENGTH - 21);
    curfile = "";
}

/ ... testing.* ... testing/ {
    printf("error at line %d: found two test results on the same line\n", NR);
    errored = 1;
}

/^ ... testing '[^']*'/ {
    if (in_test) record_unexp();
    match($0, /... testing '[^']*'/);
    curfile = substr($0, RSTART+13, RLENGTH-14);
    in_test = 1;
}

/^ ... testing with / {
    if (in_test) record_unexp();
    in_test = 1;
}

/=> passed/ {
    record_pass();
}

/=> skipped/ {
    record_skip();
}

/=> failed/ {
    record_fail();
}

/=> unexpected error/ {
    record_unexp();
}

# Not displaying "skipped" for the moment, as most of the skipped tests
# print nothing at all and are not counted.

END {
    if (errored){
        printf ("\n#### Some fatal error occurred during testing.\n\n");
        exit (3);
    }else{
        printf("\n");
        printf("Summary:\n");
        printf("  %3d test(s) passed\n", passed);
        printf("  %3d test(s) failed\n", failed);
        printf("  %3d unexpected error(s)\n", unexped);
        if (failed != 0){
            printf("\nList of failed tests:\n");
            for (i=0; i < failed; i++) printf("    %s\n", fail[i]);
        }
        if (unexped != 0){
            printf("\nList of unexpected errors:\n");
            for (i=0; i < unexped; i++) printf("    %s\n", unexp[i]);
        }
        printf("\n");
        if (failed || unexped){
            printf("#### Some tests failed. Exiting with error status.\n\n");
            exit 4;
        }
    }
}
