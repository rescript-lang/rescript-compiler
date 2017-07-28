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

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include "../config/s.h"

/*  machine-dependent definitions                              */
/*  the following definitions are for the Tahoe                */
/*  they might have to be changed for other machines           */

/*  MAXCHAR is the largest unsigned character value            */
/*  MAXSHORT is the largest value of a C short                 */
/*  MINSHORT is the most negative value of a C short           */
/*  MAXTABLE is the maximum table size                         */
/*  BITS_PER_WORD is the number of bits in a C unsigned        */
/*  WORDSIZE computes the number of words needed to            */
/*        store n bits                                         */
/*  BIT returns the value of the n-th bit starting             */
/*        from r (0-indexed)                                   */
/*  SETBIT sets the n-th bit starting from r                   */

#define        MAXCHAR                UCHAR_MAX
#define        MAXSHORT        SHRT_MAX
#define MINSHORT        SHRT_MIN
#define MAXTABLE        32500

#define BITS_PER_WORD        (8*sizeof(unsigned))
#define        WORDSIZE(n)        (((n)+(BITS_PER_WORD-1))/BITS_PER_WORD)
#define        BIT(r, n)        ((((r)[(n)/BITS_PER_WORD])>>((n)%BITS_PER_WORD))&1)
#define        SETBIT(r, n)        ((r)[(n)/BITS_PER_WORD]|=(1<<((n)%BITS_PER_WORD)))

/*  character names  */

#define        NUL                '\0'    /*  the null character  */
#define        NEWLINE                '\n'    /*  line feed  */
#define        SP                ' '     /*  space  */
#define        BS                '\b'    /*  backspace  */
#define        HT                '\t'    /*  horizontal tab  */
#define        VT                '\013'  /*  vertical tab  */
#define        CR                '\r'    /*  carriage return  */
#define        FF                '\f'    /*  form feed  */
#define        QUOTE                '\''    /*  single quote  */
#define        DOUBLE_QUOTE        '\"'    /*  double quote  */
#define        BACKSLASH        '\\'    /*  backslash  */


/* defines for constructing filenames */

#define CODE_SUFFIX        ".code.c"
#define        DEFINES_SUFFIX        ".tab.h"
#define        OUTPUT_SUFFIX        ".ml"
#define        VERBOSE_SUFFIX        ".output"
#define INTERFACE_SUFFIX ".mli"

/* keyword codes */

#define TOKEN 0
#define LEFT 1
#define RIGHT 2
#define NONASSOC 3
#define MARK 4
#define TEXT 5
#define TYPE 6
#define START 7
#define UNION 8
#define IDENT 9

/*  symbol classes  */

#define UNKNOWN 0
#define TERM 1
#define NONTERM 2


/*  the undefined value  */

#define UNDEFINED (-1)


/*  action codes  */

#define SHIFT 1
#define REDUCE 2


/*  character macros  */

#define IS_IDENT(c)       (isalnum(c) || (c) == '_' || (c) == '.' || (c) == '$')
#define IS_OCTAL(c)       ((c) >= '0' && (c) <= '7')
#define NUMERIC_VALUE(c)  ((c) - '0')


/*  symbol macros  */

#define ISTOKEN(s)        ((s) < start_symbol)
#define ISVAR(s)        ((s) >= start_symbol)


/*  storage allocation macros  */

#define CALLOC(k,n)      (calloc((unsigned)(k),(unsigned)(n)))
#define FREE(x)          (free((char*)(x)))
#define MALLOC(n)        (malloc((unsigned)(n)))
#define NEW(t)           ((t*)allocate(sizeof(t)))
#define NEW2(n,t)        ((t*)allocate((unsigned)((n)*sizeof(t))))
#define REALLOC(p,n)     (realloc((char*)(p),(unsigned)(n)))


/*  the structure of a symbol table entry  */

typedef struct bucket bucket;
struct bucket
{
    struct bucket *link;
    struct bucket *next;
    char *name;
    char *tag;
    short value;
    short index;
    short prec;
    char class;
    char assoc;
    char entry;
    char true_token;
};

/* TABLE_SIZE is the number of entries in the symbol table.      */
/* TABLE_SIZE must be a power of two.                            */

#define        TABLE_SIZE 4096

/*  the structure of the LR(0) state machine  */

typedef struct core core;
struct core
{
    struct core *next;
    struct core *link;
    short number;
    short accessing_symbol;
    short nitems;
    short items[1];
};


/*  the structure used to record shifts  */

typedef struct shifts shifts;
struct shifts
{
    struct shifts *next;
    short number;
    short nshifts;
    short shift[1];
};


/*  the structure used to store reductions  */

typedef struct reductions reductions;
struct reductions
{
    struct reductions *next;
    short number;
    short nreds;
    short rules[1];
};


/*  the structure used to represent parser actions  */

typedef struct action action;
struct action
{
    struct action *next;
    short symbol;
    short number;
    short prec;
    char action_code;
    char assoc;
    char suppressed;
};


/* global variables */

extern char dflag;
extern char lflag;
extern char rflag;
extern char tflag;
extern char vflag;
extern char qflag;
extern char sflag;
extern char big_endian;

extern char *myname;
extern char *cptr;
extern char *line;
extern int lineno;
extern char *virtual_input_file_name;
extern int outline;

extern char *action_file_name;
extern char *entry_file_name;
extern char *code_file_name;
extern char *defines_file_name;
extern char *input_file_name;
extern char *output_file_name;
extern char *text_file_name;
extern char *union_file_name;
extern char *verbose_file_name;
extern char *interface_file_name;

extern FILE *action_file;
extern FILE *entry_file;
extern FILE *code_file;
extern FILE *defines_file;
extern FILE *input_file;
extern FILE *output_file;
extern FILE *text_file;
extern FILE *union_file;
extern FILE *verbose_file;
extern FILE *interface_file;

extern int nitems;
extern int nrules;
extern int ntotalrules;
extern int nsyms;
extern int ntokens;
extern int nvars;
extern int ntags;

extern char unionized;
extern char line_format[];

extern int   start_symbol;
extern char  **symbol_name;
extern short *symbol_value;
extern short *symbol_prec;
extern char  *symbol_assoc;
extern char  **symbol_tag;
extern char  *symbol_true_token;

extern short *ritem;
extern short *rlhs;
extern short *rrhs;
extern short *rprec;
extern char  *rassoc;

extern short **derives;
extern char *nullable;

extern bucket *first_symbol;
extern bucket *last_symbol;

extern int nstates;
extern core *first_state;
extern shifts *first_shift;
extern reductions *first_reduction;
extern short *accessing_symbol;
extern core **state_table;
extern shifts **shift_table;
extern reductions **reduction_table;
extern unsigned *LA;
extern short *LAruleno;
extern short *lookaheads;
extern short *goto_map;
extern short *from_state;
extern short *to_state;

extern action **parser;
extern int SRtotal;
extern int RRtotal;
extern short *SRconflicts;
extern short *RRconflicts;
extern short *defred;
extern short *rules_used;
extern short nunused;
extern short final_state;

/* global functions */

#ifdef __GNUC__
/* Works only in GCC 2.5 and later */
#define Noreturn __attribute ((noreturn))
#else
#define Noreturn
#endif

extern char *allocate(unsigned int n);
extern bucket *lookup(char *name);
extern bucket *make_bucket(char *name);
extern action *parse_actions(register int stateno);
extern action *get_shifts(int stateno);
extern action *add_reductions(int stateno, register action *actions);
extern action *add_reduce(register action *actions, register int ruleno, register int symbol);
extern void closure (short int *nucleus, int n);
extern void create_symbol_table (void);
extern void default_action_error (void);
extern void done (int k) Noreturn;
extern void entry_without_type (char *s);
extern void fatal (char *msg);
extern void finalize_closure (void);
extern void free_parser (void);
extern void free_symbol_table (void);
extern void free_symbols (void);
extern void illegal_character (char *c_cptr);
extern void illegal_token_ref (int i, char *name);
extern void lalr (void);
extern void lr0 (void);
extern void make_parser (void);
extern void no_grammar (void);
extern void no_space (void);
extern void open_error (char *filename);
extern void output (void);
extern void over_unionized (char *u_cptr);
extern void prec_redeclared (void);
extern void polymorphic_entry_point(char *s);
extern void reader (void);
extern void reflexive_transitive_closure (unsigned int *R, int n);
extern void reprec_warning (char *s);
extern void retyped_warning (char *s);
extern void revalued_warning (char *s);
extern void set_first_derives (void);
extern void syntax_error (int st_lineno, char *st_line, char *st_cptr) Noreturn, terminal_lhs (int s_lineno);
extern void terminal_start (char *s);
extern void tokenized_start (char *s);
extern void too_many_entries (void);
extern void undefined_goal (char *s);
extern void undefined_symbol (char *s);
extern void unexpected_EOF (void);
extern void unknown_rhs (int i);
extern void unterminated_action (int a_lineno, char *a_line, char *a_cptr);
extern void unterminated_comment (int c_lineno, char *c_line, char *c_cptr);
extern void unterminated_string (int s_lineno, char *s_line, char *s_cptr);
extern void unterminated_text (int t_lineno, char *t_line, char *t_cptr);
extern void unterminated_union (int u_lineno, char *u_line, char *u_cptr);
extern void used_reserved (char *s);
extern void verbose (void);
extern void write_section (char **section);
