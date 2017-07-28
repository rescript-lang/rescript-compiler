/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* Based on public-domain code from Berkeley Yacc */

#include <signal.h>
#include <string.h>
#include "defs.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#include "version.h"

char dflag;
char lflag;
char rflag;
char tflag;
char vflag;
char qflag;
char sflag;
char big_endian;

char *file_prefix = 0;
char *myname = "yacc";
char temp_form[] = "yacc.XXXXXXX";

#ifdef NO_UNIX
char dirsep = '\\';
#else
char dirsep = '/';
#endif

int lineno;
char *virtual_input_file_name = NULL;
int outline;

char *action_file_name;
char *entry_file_name;
char *code_file_name;
char *interface_file_name;
char *defines_file_name;
char *input_file_name = "";
char *output_file_name;
char *text_file_name;
char *union_file_name;
char *verbose_file_name;

#ifdef HAS_MKSTEMP
int action_fd = -1, entry_fd = -1, text_fd = -1, union_fd = -1;
#endif

FILE *action_file;      /*  a temp file, used to save actions associated    */
                        /*  with rules until the parser is written          */
FILE *entry_file;
FILE *code_file;        /*  y.code.c (used when the -r option is specified) */
FILE *defines_file;     /*  y.tab.h                                         */
FILE *input_file;       /*  the input file                                  */
FILE *output_file;      /*  y.tab.c                                         */
FILE *text_file;        /*  a temp file, used to save text until all        */
                        /*  symbols have been defined                       */
FILE *union_file;       /*  a temp file, used to save the union             */
                        /*  definition until all symbol have been           */
                        /*  defined                                         */
FILE *verbose_file;     /*  y.output                                        */
FILE *interface_file;

int nitems;
int nrules;
int ntotalrules;
int nsyms;
int ntokens;
int nvars;

int   start_symbol;
char  **symbol_name;
short *symbol_value;
short *symbol_prec;
char  *symbol_assoc;
char **symbol_tag;
char *symbol_true_token;

short *ritem;
short *rlhs;
short *rrhs;
short *rprec;
char  *rassoc;
short **derives;
char *nullable;

#if !defined(HAS_MKSTEMP)
extern char *mktemp(char *);
#endif
#ifndef NO_UNIX
extern char *getenv(const char *);
#endif


void done(int k)
{
#ifdef HAS_MKSTEMP
    if (action_fd != -1)
       unlink(action_file_name);
    if (entry_fd != -1)
       unlink(entry_file_name);
    if (text_fd != -1)
       unlink(text_file_name);
    if (union_fd != -1)
       unlink(union_file_name);
#else
    if (action_file) { fclose(action_file); unlink(action_file_name); }
    if (entry_file) { fclose(entry_file); unlink(entry_file_name); }
    if (text_file) { fclose(text_file); unlink(text_file_name); }
    if (union_file) { fclose(union_file); unlink(union_file_name); }
#endif
    if (output_file && k > 0) {
      fclose(output_file); unlink(output_file_name);
    }
    if (interface_file && k > 0) {
      fclose(interface_file); unlink(interface_file_name);
    }
    exit(k);
}


void onintr(int dummy)
{
    done(1);
}


void set_signals(void)
{
#ifdef SIGINT
    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
        signal(SIGINT, onintr);
#endif
#ifdef SIGTERM
    if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
        signal(SIGTERM, onintr);
#endif
#ifdef SIGHUP
    if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
        signal(SIGHUP, onintr);
#endif
}


void usage(void)
{
    fprintf(stderr, "usage: %s [-v] [-q] [-b file_prefix] filename\n",
            myname);
    exit(1);
}

void getargs(int argc, char **argv)
{
    register int i;
    register char *s;

    if (argc > 0) myname = argv[0];
    for (i = 1; i < argc; ++i)
    {
        s = argv[i];
        if (*s != '-') break;
        switch (*++s)
        {
        case '\0':
            input_file = stdin;
            file_prefix = "stdin";
            if (i + 1 < argc) usage();
            return;

        case '-':
            ++i;
            goto no_more_options;

        case 'v':
            if (!strcmp (argv[i], "-version")){
              printf ("The OCaml parser generator, version "
                      OCAML_VERSION "\n");
              exit (0);
            }else if (!strcmp (argv[i], "-vnum")){
              printf (OCAML_VERSION "\n");
              exit (0);
            }else{
              vflag = 1;
            }
            break;

        case 'q':
            qflag = 1;
            break;

        case 'b':
            if (*++s)
                 file_prefix = s;
            else if (++i < argc)
                file_prefix = argv[i];
            else
                usage();
            continue;

        default:
            usage();
        }

        for (;;)
        {
            switch (*++s)
            {
            case '\0':
                goto end_of_option;

            case 'v':
                vflag = 1;
                break;

            case 'q':
                qflag = 1;
                break;

            default:
                usage();
            }
        }
end_of_option:;
    }

no_more_options:;
    if (i + 1 != argc) usage();
    input_file_name = argv[i];
    if (file_prefix == 0) {
      int len;
      len = strlen(argv[i]);
      file_prefix = malloc(len + 1);
      if (file_prefix == 0) no_space();
      strcpy(file_prefix, argv[i]);
      while (len > 0) {
        len--;
        if (file_prefix[len] == '.') {
          file_prefix[len] = 0;
          break;
        }
      }
    }
}


char *
allocate(unsigned int n)
{
    register char *p;

    p = NULL;
    if (n)
    {
        p = CALLOC(1, n);
        if (!p) no_space();
    }
    return (p);
}


void create_file_names(void)
{
    int i, len;
    char *tmpdir;

#ifdef NO_UNIX
    tmpdir = getenv("TEMP");
    if (tmpdir == 0) tmpdir = ".";
#else
    tmpdir = getenv("TMPDIR");
    if (tmpdir == 0) tmpdir = "/tmp";
#endif
    len = strlen(tmpdir);
    i = len + sizeof(temp_form);
    if (len && tmpdir[len-1] != dirsep)
        ++i;

    action_file_name = MALLOC(i);
    if (action_file_name == 0) no_space();
    entry_file_name = MALLOC(i);
    if (entry_file_name == 0) no_space();
    text_file_name = MALLOC(i);
    if (text_file_name == 0) no_space();
    union_file_name = MALLOC(i);
    if (union_file_name == 0) no_space();

    strcpy(action_file_name, tmpdir);
    strcpy(entry_file_name, tmpdir);
    strcpy(text_file_name, tmpdir);
    strcpy(union_file_name, tmpdir);

    if (len && tmpdir[len - 1] != dirsep)
    {
        action_file_name[len] = dirsep;
        entry_file_name[len] = dirsep;
        text_file_name[len] = dirsep;
        union_file_name[len] = dirsep;
        ++len;
    }

    strcpy(action_file_name + len, temp_form);
    strcpy(entry_file_name + len, temp_form);
    strcpy(text_file_name + len, temp_form);
    strcpy(union_file_name + len, temp_form);

    action_file_name[len + 5] = 'a';
    entry_file_name[len + 5] = 'e';
    text_file_name[len + 5] = 't';
    union_file_name[len + 5] = 'u';

#ifdef HAS_MKSTEMP
    action_fd = mkstemp(action_file_name);
    if (action_fd == -1)
        open_error(action_file_name);
    entry_fd = mkstemp(entry_file_name);
    if (entry_fd == -1)
        open_error(entry_file_name);
    text_fd = mkstemp(text_file_name);
    if (text_fd == -1)
        open_error(text_file_name);
    union_fd = mkstemp(union_file_name);
    if (union_fd == -1)
        open_error(union_file_name);
#else
    mktemp(action_file_name);
    mktemp(entry_file_name);
    mktemp(text_file_name);
    mktemp(union_file_name);
#endif

    len = strlen(file_prefix);

    output_file_name = MALLOC(len + 7);
    if (output_file_name == 0)
        no_space();
    strcpy(output_file_name, file_prefix);
    strcpy(output_file_name + len, OUTPUT_SUFFIX);

    code_file_name = output_file_name;

    if (vflag)
    {
        verbose_file_name = MALLOC(len + 8);
        if (verbose_file_name == 0)
            no_space();
        strcpy(verbose_file_name, file_prefix);
        strcpy(verbose_file_name + len, VERBOSE_SUFFIX);
    }

    interface_file_name = MALLOC(len + 8);
    if (interface_file_name == 0)
        no_space();
    strcpy(interface_file_name, file_prefix);
    strcpy(interface_file_name + len, INTERFACE_SUFFIX);

}


void open_files(void)
{
    create_file_names();

    if (input_file == 0)
    {
        input_file = fopen(input_file_name, "r");
        if (input_file == 0)
            open_error(input_file_name);
    }

#ifdef HAS_MKSTEMP
    action_file = fdopen(action_fd, "w");
#else
    action_file = fopen(action_file_name, "w");
#endif
    if (action_file == 0)
        open_error(action_file_name);

#ifdef HAS_MKSTEMP
    entry_file = fdopen(entry_fd, "w");
#else
    entry_file = fopen(entry_file_name, "w");
#endif
    if (entry_file == 0)
        open_error(entry_file_name);

#ifdef HAS_MKSTEMP
    text_file = fdopen(text_fd, "w");
#else
    text_file = fopen(text_file_name, "w");
#endif
    if (text_file == 0)
        open_error(text_file_name);

    if (vflag)
    {
        verbose_file = fopen(verbose_file_name, "w");
        if (verbose_file == 0)
            open_error(verbose_file_name);
    }

    if (dflag)
    {
        defines_file = fopen(defines_file_name, "w");
        if (defines_file == 0)
            open_error(defines_file_name);
#ifdef HAS_MKSTEMP
        union_file = fdopen(union_fd, "w");
#else
        union_file = fopen(union_file_name, "w");
#endif
        if (union_file ==  0)
            open_error(union_file_name);
    }

    output_file = fopen(output_file_name, "w");
    if (output_file == 0)
        open_error(output_file_name);

    if (rflag)
    {
        code_file = fopen(code_file_name, "w");
        if (code_file == 0)
            open_error(code_file_name);
    }
    else
        code_file = output_file;


    interface_file = fopen(interface_file_name, "w");
    if (interface_file == 0)
      open_error(interface_file_name);
}

int main(int argc, char **argv)
{
    set_signals();
    getargs(argc, argv);
    open_files();
    reader();
    lr0();
    lalr();
    make_parser();
    verbose();
    output();
    done(0);
    /*NOTREACHED*/
    return 0;
}
