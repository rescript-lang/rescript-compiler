#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*         Damien Doligez, projet Gallium, INRIA Rocquencourt             *
#*                                                                        *
#*   Copyright 2013 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

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
    if (!(key in RESULTS)) ++nresults;
    RESULTS[key] = "p";
    delete SKIPPED[curdir];
    clear();
}

function record_skip() {
    check();
    if (!(key in RESULTS)) ++nresults;
    RESULTS[key] = "s";
    if (curdir in SKIPPED) SKIPPED[curdir] = 1;
    clear();
}

# The output cares only if the test passes at least once so if a test passes,
# but then fails in a re-run triggered by a different test, ignore it.
function record_fail() {
    check();
    if (!(key in RESULTS) || RESULTS[key] == "s"){
        if (!(key in RESULTS)) ++nresults;
        RESULTS[key] = "f";
    }
    delete SKIPPED[curdir];
    clear();
}

function record_unexp() {
    if (!(key in RESULTS) || RESULTS[key] == "s"){
        if (!(key in RESULTS)) ++nresults;
        RESULTS[key] = "e";
    }
    delete SKIPPED[curdir];
    clear();
}

/Running tests from '[^']*'/ {
    if (in_test) record_unexp();
    match($0, /Running tests from '[^']*'/);
    curdir = substr($0, RSTART+20, RLENGTH - 21);
    # Use SKIPPED[curdir] as a sentinel to detect no output
    SKIPPED[curdir] = 0;
    key = curdir;
    DIRS[key] = key;
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
    if (match($0, /... testing '[^']*' with [^:=]*/)){
        curfile = substr($0, RSTART+12, RLENGTH-12);
    }
    key = sprintf ("%s/%s", curdir, curfile);
    DIRS[key] = curdir;
    in_test = 1;
}

/^ ... testing (with|[^'])/ {
    if (in_test) record_unexp();
    key = curdir;
    DIRS[key] = curdir;
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

/^re-ran / {
    if (in_test){
        printf("error at line %d: found re-ran inside a test\n", NR);
        errored = 1;
    }else{
        RERAN[substr($0, 8, length($0)-7)] += 1;
        ++ reran;
    }
}

END {
    if (errored){
        printf ("\n#### Some fatal error occurred during testing.\n\n");
        exit (3);
    }else{
        if (!retries){
            for (key in SKIPPED){
                if (!SKIPPED[key]){
                    ++ empty;
                    blanks[emptyidx++] = key;
                    delete SKIPPED[key];
                }
            }
            for (key in RESULTS){
                r = RESULTS[key];
                if (r == "p"){
                    ++ passed;
                }else if (r == "f"){
                    ++ failed;
                    fail[failidx++] = key;
                }else if (r == "e"){
                    ++ unexped;
                    unexp[unexpidx++] = key;
                }else if (r == "s"){
                    ++ skipped;
                    curdir = DIRS[key];
                    if (curdir in SKIPPED){
                        if (SKIPPED[curdir]){
                            SKIPPED[curdir] = 0;
                            skips[skipidx++] = curdir;
                        }
                    }else{
                        skips[skipidx++] = key;
                    }
                }
            }
            printf("\n");
            printf("Summary:\n");
            printf("  %3d tests passed\n", passed);
            printf("  %3d tests skipped\n", skipped);
            printf("  %3d tests failed\n", failed);
            printf("  %3d unexpected errors\n", unexped);
            printf("  %3d tests considered", nresults);
            if (nresults == passed + skipped + failed + unexped){
                printf ("\n");
            }else{
                printf (" (totals don't add up??)");
            }
            if (reran != 0){
                printf("  %3d test dir re-runs\n", reran);
            }
            if (failed != 0){
                printf("\nList of failed tests:\n");
                for (i=0; i < failed; i++) printf("    %s\n", fail[i]);
            }
            if (unexped != 0){
                printf("\nList of unexpected errors:\n");
                for (i=0; i < unexped; i++) printf("    %s\n", unexp[i]);
            }
            if (skipped != 0){
                printf("\nList of skipped tests:\n");
                for (i=0; i < skipidx; i++) printf("    %s\n", skips[i]);
            }
            if (empty != 0){
                printf("\nList of directories returning no results:\n");
                for (i=0; i < empty; i++) printf("    %s\n", blanks[i]);
            }
            printf("\n");
            if (failed || unexped){
                printf("#### Something failed. Exiting with error status.\n\n");
                exit 4;
            }
        }else{
            for (key in RESULTS){
                if (RESULTS[key] == "f" || RESULTS[key] == "e"){
                    key = DIRS[key];
                    if (!(key in RERUNS)){
                        RERUNS[key] = 1;
                        if (RERAN[key] < max_retries){
                            printf("%s\n", key);
                        }
                    }
                }
            }
        }
    }
}
