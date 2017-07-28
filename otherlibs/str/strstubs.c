/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <string.h>
#include <ctype.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

/* The backtracking NFA interpreter */

union backtrack_point {
  struct {
    value * pc;                 /* with low bit set */
    unsigned char * txt;
  } pos;
  struct {
    unsigned char ** loc;       /* with low bit clear */
    unsigned char * val;
  } undo;
};

#define Set_tag(p) ((value *) ((intnat)(p) | 1))
#define Clear_tag(p) ((value *) ((intnat)(p) & ~1))
#define Tag_is_set(p) ((intnat)(p) & 1)

#define BACKTRACK_STACK_BLOCK_SIZE 500

struct backtrack_stack {
  struct backtrack_stack * previous;
  union backtrack_point point[BACKTRACK_STACK_BLOCK_SIZE];
};

#define Opcode(x) ((x) & 0xFF)
#define Arg(x) ((uintnat)(x) >> 8)
#define SignedArg(x) ((intnat)(x) >> 8)

enum {
  CHAR,       /* match a single character */
  CHARNORM,   /* match a single character, after normalization */
  STRING,     /* match a character string */
  STRINGNORM, /* match a character string, after normalization */
  CHARCLASS,  /* match a character class */
  BOL,        /* match at beginning of line */
  EOL,        /* match at end of line */
  WORDBOUNDARY, /* match on a word boundary */
  BEGGROUP,   /* record the beginning of a group */
  ENDGROUP,   /* record the end of a group */
  REFGROUP,   /* match a previously matched group */
  ACCEPT,     /* report success */
  SIMPLEOPT,  /* match a character class 0 or 1 times */
  SIMPLESTAR, /* match a character class 0, 1 or several times */
  SIMPLEPLUS, /* match a character class 1 or several times */
  GOTO,       /* unconditional branch */
  PUSHBACK,   /* record a backtrack point --
                 where to jump in case of failure */
  SETMARK,    /* remember current position in given register # */
  CHECKPROGRESS /* backtrack if no progress was made w.r.t. reg # */
};

/* Accessors in a compiled regexp */
#define Prog(re) Field(re, 0)
#define Cpool(re) Field(re, 1)
#define Normtable(re) Field(re, 2)
#define Numgroups(re) Int_val(Field(re, 3))
#define Numregisters(re) Int_val(Field(re, 4))
#define Startchars(re) Int_val(Field(re, 5))

/* Record positions of matched groups */
#define NUM_GROUPS 32
struct re_group {
  unsigned char * start;
  unsigned char * end;
};
static struct re_group re_group[NUM_GROUPS];

/* Record positions reached during matching; used to check progress
   in repeated matching of a regexp. */
#define NUM_REGISTERS 64
static unsigned char * re_register[NUM_REGISTERS];

/* The initial backtracking stack */
static struct backtrack_stack initial_stack = { NULL, };

/* Free a chained list of backtracking stacks */
static void free_backtrack_stack(struct backtrack_stack * stack)
{
  struct backtrack_stack * prevstack;
  while ((prevstack = stack->previous) != NULL) {
    stat_free(stack);
    stack = prevstack;
  }
}

/* Membership in a bit vector representing a set of booleans */
#define In_bitset(s,i,tmp) (tmp = (i), ((s)[tmp >> 3] >> (tmp & 7)) & 1)

/* Determine if a character is a word constituent */
/* PR#4874: word constituent = letter, digit, underscore. */

static unsigned char re_word_letters[32] = {
  0x00, 0x00, 0x00, 0x00,       /* 0x00-0x1F: none */
  0x00, 0x00, 0xFF, 0x03,       /* 0x20-0x3F: digits 0-9 */
  0xFE, 0xFF, 0xFF, 0x87,       /* 0x40-0x5F: A to Z, _ */
  0xFE, 0xFF, 0xFF, 0x07,       /* 0x60-0x7F: a to z */
  0x00, 0x00, 0x00, 0x00,       /* 0x80-0x9F: none */
  0x00, 0x00, 0x00, 0x00,       /* 0xA0-0xBF: none */
  0xFF, 0xFF, 0x7F, 0xFF,       /* 0xC0-0xDF: Latin-1 accented uppercase */
  0xFF, 0xFF, 0x7F, 0xFF        /* 0xE0-0xFF: Latin-1 accented lowercase */
};

#define Is_word_letter(c) ((re_word_letters[(c) >> 3] >> ((c) & 7)) & 1)

/* The bytecode interpreter for the NFA */
static int re_match(value re,
                    unsigned char * starttxt,
                    register unsigned char * txt,
                    register unsigned char * endtxt,
                    int accept_partial_match)
{
  register value * pc;
  intnat instr;
  struct backtrack_stack * stack;
  union backtrack_point * sp;
  value cpool;
  value normtable;
  unsigned char c;
  union backtrack_point back;

  { int i;
    struct re_group * p;
    unsigned char ** q;
    for (p = &re_group[1], i = Numgroups(re); i > 1; i--, p++)
      p->start = p->end = NULL;
    for (q = &re_register[0], i = Numregisters(re); i > 0; i--, q++)
      *q = NULL;
  }

  pc = &Field(Prog(re), 0);
  stack = &initial_stack;
  sp = stack->point;
  cpool = Cpool(re);
  normtable = Normtable(re);
  re_group[0].start = txt;

  while (1) {
    instr = Long_val(*pc++);
    switch (Opcode(instr)) {
    case CHAR:
      if (txt == endtxt) goto prefix_match;
      if (*txt != Arg(instr)) goto backtrack;
      txt++;
      break;
    case CHARNORM:
      if (txt == endtxt) goto prefix_match;
      if (Byte_u(normtable, *txt) != Arg(instr)) goto backtrack;
      txt++;
      break;
    case STRING: {
      unsigned char * s =
        (unsigned char *) String_val(Field(cpool, Arg(instr)));
      while ((c = *s++) != 0) {
        if (txt == endtxt) goto prefix_match;
        if (c != *txt) goto backtrack;
        txt++;
      }
      break;
    }
    case STRINGNORM: {
      unsigned char * s =
        (unsigned char *) String_val(Field(cpool, Arg(instr)));
      while ((c = *s++) != 0) {
        if (txt == endtxt) goto prefix_match;
        if (c != Byte_u(normtable, *txt)) goto backtrack;
        txt++;
      }
      break;
    }
    case CHARCLASS:
      if (txt == endtxt) goto prefix_match;
      if (! In_bitset(String_val(Field(cpool, Arg(instr))), *txt, c))
        goto backtrack;
      txt++;
      break;
    case BOL:
      if (txt > starttxt && txt[-1] != '\n') goto backtrack;
      break;
    case EOL:
      if (txt < endtxt && *txt != '\n') goto backtrack;
      break;
    case WORDBOUNDARY:
      /* At beginning and end of text: no
         At beginning of text: OK if current char is a letter
         At end of text: OK if previous char is a letter
         Otherwise:
           OK if previous char is a letter and current char not a letter
           or previous char is not a letter and current char is a letter */
      if (txt == starttxt) {
        if (txt == endtxt) goto prefix_match;
        if (Is_word_letter(txt[0])) break;
        goto backtrack;
      } else if (txt == endtxt) {
        if (Is_word_letter(txt[-1])) break;
        goto backtrack;
      } else {
        if (Is_word_letter(txt[-1]) != Is_word_letter(txt[0])) break;
        goto backtrack;
      }
    case BEGGROUP: {
      int group_no = Arg(instr);
      struct re_group * group = &(re_group[group_no]);
      back.undo.loc = &(group->start);
      back.undo.val = group->start;
      group->start = txt;
      goto push;
    }
    case ENDGROUP: {
      int group_no = Arg(instr);
      struct re_group * group = &(re_group[group_no]);
      back.undo.loc = &(group->end);
      back.undo.val = group->end;
      group->end = txt;
      goto push;
    }
    case REFGROUP: {
      int group_no = Arg(instr);
      struct re_group * group = &(re_group[group_no]);
      unsigned char * s;
      if (group->start == NULL || group->end == NULL) goto backtrack;
      for (s = group->start; s < group->end; s++) {
        if (txt == endtxt) goto prefix_match;
        if (*s != *txt) goto backtrack;
        txt++;
      }
      break;
    }
    case ACCEPT:
      goto accept;
    case SIMPLEOPT: {
      char * set = String_val(Field(cpool, Arg(instr)));
      if (txt < endtxt && In_bitset(set, *txt, c)) txt++;
      break;
    }
    case SIMPLESTAR: {
      char * set = String_val(Field(cpool, Arg(instr)));
      while (txt < endtxt && In_bitset(set, *txt, c))
        txt++;
      break;
    }
    case SIMPLEPLUS: {
      char * set = String_val(Field(cpool, Arg(instr)));
      if (txt == endtxt) goto prefix_match;
      if (! In_bitset(set, *txt, c)) goto backtrack;
      txt++;
      while (txt < endtxt && In_bitset(set, *txt, c))
        txt++;
      break;
    }
    case GOTO:
      pc = pc + SignedArg(instr);
      break;
    case PUSHBACK:
      back.pos.pc = Set_tag(pc + SignedArg(instr));
      back.pos.txt = txt;
      goto push;
    case SETMARK: {
      int reg_no = Arg(instr);
      unsigned char ** reg = &(re_register[reg_no]);
      back.undo.loc = reg;
      back.undo.val = *reg;
      *reg = txt;
      goto push;
    }
    case CHECKPROGRESS: {
      int reg_no = Arg(instr);
      if (re_register[reg_no] == txt)
        goto backtrack;
      break;
    }
    default:
      caml_fatal_error ("impossible case in re_match");
    }
    /* Continue with next instruction */
    continue;

  push:
    /* Push an item on the backtrack stack and continue with next instr */
    if (sp == stack->point + BACKTRACK_STACK_BLOCK_SIZE) {
      struct backtrack_stack * newstack =
        caml_stat_alloc(sizeof(struct backtrack_stack));
      newstack->previous = stack;
      stack = newstack;
      sp = stack->point;
    }
    *sp = back;
    sp++;
    continue;

  prefix_match:
    /* We get here when matching failed because the end of text
       was encountered. */
    if (accept_partial_match) goto accept;

  backtrack:
    /* We get here when matching fails.  Backtrack to most recent saved
       program point, undoing variable assignments on the way. */
    while (1) {
      if (sp == stack->point) {
        struct backtrack_stack * prevstack = stack->previous;
        if (prevstack == NULL) return 0;
        stat_free(stack);
        stack = prevstack;
        sp = stack->point + BACKTRACK_STACK_BLOCK_SIZE;
      }
      sp--;
      if (Tag_is_set(sp->pos.pc)) {
        pc = Clear_tag(sp->pos.pc);
        txt = sp->pos.txt;
        break;
      } else {
        *(sp->undo.loc) = sp->undo.val;
      }
    }
    continue;
  }

 accept:
  /* We get here when the regexp was successfully matched */
  free_backtrack_stack(stack);
  re_group[0].end = txt;
  return 1;
}

/* Allocate an integer array containing the positions of the matched groups.
   Beginning of group #N is at 2N, end is at 2N+1.
   Take position = -1 when group wasn't matched. */

static value re_alloc_groups(value re, value str)
{
  CAMLparam1(str);
  CAMLlocal1(res);
  unsigned char * starttxt = (unsigned char *) String_val(str);
  int n = Numgroups(re);
  int i;
  struct re_group * group;

  res = alloc(n * 2, 0);
  for (i = 0; i < n; i++) {
    group = &(re_group[i]);
    if (group->start == NULL || group->end == NULL) {
      Field(res, i * 2) = Val_int(-1);
      Field(res, i * 2 + 1) = Val_int(-1);
    } else {
      Field(res, i * 2) = Val_long(group->start - starttxt);
      Field(res, i * 2 + 1) = Val_long(group->end - starttxt);
    }
  }
  CAMLreturn(res);
}

/* String matching and searching.  All functions return the empty array
   on failure, and an array of positions on success. */

CAMLprim value re_string_match(value re, value str, value pos)
{
  unsigned char * starttxt = &Byte_u(str, 0);
  unsigned char * txt = &Byte_u(str, Long_val(pos));
  unsigned char * endtxt = &Byte_u(str, string_length(str));

  if (txt < starttxt || txt > endtxt)
    invalid_argument("Str.string_match");
  if (re_match(re, starttxt, txt, endtxt, 0)) {
    return re_alloc_groups(re, str);
  } else {
    return Atom(0);
  }
}

CAMLprim value re_partial_match(value re, value str, value pos)
{
  unsigned char * starttxt = &Byte_u(str, 0);
  unsigned char * txt = &Byte_u(str, Long_val(pos));
  unsigned char * endtxt = &Byte_u(str, string_length(str));

  if (txt < starttxt || txt > endtxt)
    invalid_argument("Str.string_partial_match");
  if (re_match(re, starttxt, txt, endtxt, 1)) {
    return re_alloc_groups(re, str);
  } else {
    return Atom(0);
  }
}

CAMLprim value re_search_forward(value re, value str, value startpos)
{
  unsigned char * starttxt = &Byte_u(str, 0);
  unsigned char * txt = &Byte_u(str, Long_val(startpos));
  unsigned char * endtxt = &Byte_u(str, string_length(str));
  unsigned char * startchars;

  if (txt < starttxt || txt > endtxt)
    invalid_argument("Str.search_forward");
  if (Startchars(re) == -1) {
    do {
      if (re_match(re, starttxt, txt, endtxt, 0))
        return re_alloc_groups(re, str);
      txt++;
    } while (txt <= endtxt);
    return Atom(0);
  } else {
    startchars =
      (unsigned char *) String_val(Field(Cpool(re), Startchars(re)));
    do {
      while (txt < endtxt && startchars[*txt] == 0) txt++;
      if (re_match(re, starttxt, txt, endtxt, 0))
        return re_alloc_groups(re, str);
      txt++;
    } while (txt <= endtxt);
    return Atom(0);
  }
}

CAMLprim value re_search_backward(value re, value str, value startpos)
{
  unsigned char * starttxt = &Byte_u(str, 0);
  unsigned char * txt = &Byte_u(str, Long_val(startpos));
  unsigned char * endtxt = &Byte_u(str, string_length(str));
  unsigned char * startchars;

  if (txt < starttxt || txt > endtxt)
    invalid_argument("Str.search_backward");
  if (Startchars(re) == -1) {
    do {
      if (re_match(re, starttxt, txt, endtxt, 0))
        return re_alloc_groups(re, str);
      txt--;
    } while (txt >= starttxt);
    return Atom(0);
  } else {
    startchars =
      (unsigned char *) String_val(Field(Cpool(re), Startchars(re)));
    do {
      while (txt > starttxt && startchars[*txt] == 0) txt--;
      if (re_match(re, starttxt, txt, endtxt, 0))
        return re_alloc_groups(re, str);
      txt--;
    } while (txt >= starttxt);
    return Atom(0);
  }
}

/* Replacement */

CAMLprim value re_replacement_text(value repl, value groups, value orig)
{
  CAMLparam3(repl, groups, orig);
  CAMLlocal1(res);
  mlsize_t start, end, len, n;
  char * p, * q;
  int c;

  len = 0;
  p = String_val(repl);
  n = string_length(repl);
  while (n > 0) {
    c = *p++; n--;
    if(c != '\\')
      len++;
    else {
      if (n == 0) failwith("Str.replace: illegal backslash sequence");
      c = *p++; n--;
      switch (c) {
      case '\\':
        len++; break;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        c -= '0';
        if (c*2 >= Wosize_val(groups))
          failwith("Str.replace: reference to unmatched group");
        start = Long_val(Field(groups, c*2));
        end = Long_val(Field(groups, c*2 + 1));
        if (start == (mlsize_t) -1)
          failwith("Str.replace: reference to unmatched group");
        len += end - start;
        break;
      default:
        len += 2; break;
      }
    }
  }
  res = alloc_string(len);
  p = String_val(repl);
  q = String_val(res);
  n = string_length(repl);
  while (n > 0) {
    c = *p++; n--;
    if(c != '\\')
      *q++ = c;
    else {
      c = *p++; n--;
      switch (c) {
      case '\\':
        *q++ = '\\'; break;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        c -= '0';
        start = Long_val(Field(groups, c*2));
        end = Long_val(Field(groups, c*2 + 1));
        len = end - start;
        memmove (q, &Byte(orig, start), len);
        q += len;
        break;
      default:
        *q++ = '\\'; *q++ = c; break;
      }
    }
  }
  CAMLreturn(res);
}
