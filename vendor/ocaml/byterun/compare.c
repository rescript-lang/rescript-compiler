/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <string.h>
#include <stdlib.h>
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

#if defined(LACKS_SANE_NAN) && !defined(isnan)
#define isnan _isnan
#endif

/* Structural comparison on trees. */

struct compare_item { value * v1, * v2; mlsize_t count; };

#define COMPARE_STACK_INIT_SIZE 8
#define COMPARE_STACK_MIN_ALLOC_SIZE 32
#define COMPARE_STACK_MAX_SIZE (1024*1024)
CAMLexport int caml_compare_unordered;

struct compare_stack {
  struct compare_item init_stack[COMPARE_STACK_INIT_SIZE];
  struct compare_item* stack;
  struct compare_item* limit;
};

/* Free the compare stack if needed */
static void compare_free_stack(struct compare_stack* stk)
{
  if (stk->stack != stk->init_stack) {
    caml_stat_free(stk->stack);
    stk->stack = NULL;
  }
}

/* Same, then raise Out_of_memory */
static void compare_stack_overflow(struct compare_stack* stk)
{
  caml_gc_message (0x04, "Stack overflow in structural comparison\n");
  compare_free_stack(stk);
  caml_raise_out_of_memory();
}

/* Grow the compare stack */
static struct compare_item * compare_resize_stack(struct compare_stack* stk,
                                                  struct compare_item * sp)
{
  asize_t newsize;
  asize_t sp_offset = sp - stk->stack;
  struct compare_item * newstack;

  if (stk->stack == stk->init_stack) {
    newsize = COMPARE_STACK_MIN_ALLOC_SIZE;
    newstack = caml_stat_alloc_noexc(sizeof(struct compare_item) * newsize);
    if (newstack == NULL) compare_stack_overflow(stk);
    memcpy(newstack, stk->init_stack,
           sizeof(struct compare_item) * COMPARE_STACK_INIT_SIZE);
  } else {
    newsize = 2 * (stk->limit - stk->stack);
    if (newsize >= COMPARE_STACK_MAX_SIZE) compare_stack_overflow(stk);
    newstack = caml_stat_resize_noexc(stk->stack,
                                      sizeof(struct compare_item) * newsize);
    if (newstack == NULL) compare_stack_overflow(stk);
  }
  stk->stack = newstack;
  stk->limit = newstack + newsize;
  return newstack + sp_offset;
}


static intnat do_compare_val(struct compare_stack* stk,
                             value v1, value v2, int total);

static intnat compare_val(value v1, value v2, int total)
{
  struct compare_stack stk;
  intnat res;
  stk.stack = stk.init_stack;
  stk.limit = stk.stack + COMPARE_STACK_INIT_SIZE;
  res = do_compare_val(&stk, v1, v2, total);
  compare_free_stack(&stk);
  return res;
}

/* Structural comparison */


#define LESS -1
#define EQUAL 0
#define GREATER 1
#define UNORDERED ((intnat)1 << (8 * sizeof(value) - 1))

/* The return value of compare_val is as follows:
      > 0                 v1 is greater than v2
      0                   v1 is equal to v2
      < 0 and > UNORDERED v1 is less than v2
      UNORDERED           v1 and v2 cannot be compared */

static intnat do_compare_val(struct compare_stack* stk,
                             value v1, value v2, int total)
{
  struct compare_item * sp;
  tag_t t1, t2;

  sp = stk->stack;
  while (1) {
    if (v1 == v2 && total) goto next_item;
    if (Is_long(v1)) {
      if (v1 == v2) goto next_item;
      if (Is_long(v2))
        return Long_val(v1) - Long_val(v2);
      /* Subtraction above cannot overflow and cannot result in UNORDERED */
      if (Is_in_value_area(v2)) {
        switch (Tag_val(v2)) {
        case Forward_tag:
          v2 = Forward_val(v2);
          continue;
        case Custom_tag: {
          int res;
          int (*compare)(value v1, value v2) = Custom_ops_val(v2)->compare_ext;
          if (compare == NULL) break;  /* for backward compatibility */
          caml_compare_unordered = 0;
          res = compare(v1, v2);
          if (caml_compare_unordered && !total) return UNORDERED;
          if (res != 0) return res;
          goto next_item;
        }
        default: /*fallthrough*/;
        }
      }
      return LESS;                /* v1 long < v2 block */
    }
    if (Is_long(v2)) {
      if (Is_in_value_area(v1)) {
        switch (Tag_val(v1)) {
        case Forward_tag:
          v1 = Forward_val(v1);
          continue;
        case Custom_tag: {
          int res;
          int (*compare)(value v1, value v2) = Custom_ops_val(v1)->compare_ext;
          if (compare == NULL) break;  /* for backward compatibility */
          caml_compare_unordered = 0;
          res = compare(v1, v2);
          if (caml_compare_unordered && !total) return UNORDERED;
          if (res != 0) return res;
          goto next_item;
        }
        default: /*fallthrough*/;
        }
      }
      return GREATER;            /* v1 block > v2 long */
    }
    /* If one of the objects is outside the heap (but is not an atom),
       use address comparison. Since both addresses are 2-aligned,
       shift lsb off to avoid overflow in subtraction. */
    if (! Is_in_value_area(v1) || ! Is_in_value_area(v2)) {
      if (v1 == v2) goto next_item;
      return (v1 >> 1) - (v2 >> 1);
      /* Subtraction above cannot result in UNORDERED */
    }
    t1 = Tag_val(v1);
    t2 = Tag_val(v2);
    if (t1 == Forward_tag) { v1 = Forward_val (v1); continue; }
    if (t2 == Forward_tag) { v2 = Forward_val (v2); continue; }
    if (t1 != t2) return (intnat)t1 - (intnat)t2;
    switch(t1) {
    case String_tag: {
      mlsize_t len1, len2;
      int res;
      if (v1 == v2) break;
      len1 = caml_string_length(v1);
      len2 = caml_string_length(v2);
      res = memcmp(String_val(v1), String_val(v2), len1 <= len2 ? len1 : len2);
      if (res < 0) return LESS;
      if (res > 0) return GREATER;
      if (len1 != len2) return len1 - len2;
      break;
    }
    case Double_tag: {
      double d1 = Double_val(v1);
      double d2 = Double_val(v2);
#ifdef LACKS_SANE_NAN
      if (isnan(d2)) {
        if (! total) return UNORDERED;
        if (isnan(d1)) break;
        return GREATER;
      } else if (isnan(d1)) {
        if (! total) return UNORDERED;
        return LESS;
      }
#endif
      if (d1 < d2) return LESS;
      if (d1 > d2) return GREATER;
#ifndef LACKS_SANE_NAN
      if (d1 != d2) {
        if (! total) return UNORDERED;
        /* One or both of d1 and d2 is NaN.  Order according to the
           convention NaN = NaN and NaN < f for all other floats f. */
        if (d1 == d1) return GREATER; /* d1 is not NaN, d2 is NaN */
        if (d2 == d2) return LESS;    /* d2 is not NaN, d1 is NaN */
        /* d1 and d2 are both NaN, thus equal: continue comparison */
      }
#endif
      break;
    }
    case Double_array_tag: {
      mlsize_t sz1 = Wosize_val(v1) / Double_wosize;
      mlsize_t sz2 = Wosize_val(v2) / Double_wosize;
      mlsize_t i;
      if (sz1 != sz2) return sz1 - sz2;
      for (i = 0; i < sz1; i++) {
        double d1 = Double_flat_field(v1, i);
        double d2 = Double_flat_field(v2, i);
  #ifdef LACKS_SANE_NAN
        if (isnan(d2)) {
          if (! total) return UNORDERED;
          if (isnan(d1)) break;
          return GREATER;
        } else if (isnan(d1)) {
          if (! total) return UNORDERED;
          return LESS;
        }
  #endif
        if (d1 < d2) return LESS;
        if (d1 > d2) return GREATER;
  #ifndef LACKS_SANE_NAN
        if (d1 != d2) {
          if (! total) return UNORDERED;
          /* See comment for Double_tag case */
          if (d1 == d1) return GREATER;
          if (d2 == d2) return LESS;
        }
  #endif
      }
      break;
    }
    case Abstract_tag:
      compare_free_stack(stk);
      caml_invalid_argument("compare: abstract value");
    case Closure_tag:
    case Infix_tag:
      compare_free_stack(stk);
      caml_invalid_argument("compare: functional value");
    case Object_tag: {
      intnat oid1 = Oid_val(v1);
      intnat oid2 = Oid_val(v2);
      if (oid1 != oid2) return oid1 - oid2;
      break;
    }
    case Custom_tag: {
      int res;
      int (*compare)(value v1, value v2) = Custom_ops_val(v1)->compare;
      /* Hardening against comparisons between different types */
      if (compare != Custom_ops_val(v2)->compare) {
        return strcmp(Custom_ops_val(v1)->identifier,
                      Custom_ops_val(v2)->identifier) < 0
               ? LESS : GREATER;
      }
      if (compare == NULL) {
        compare_free_stack(stk);
        caml_invalid_argument("compare: abstract value");
      }
      caml_compare_unordered = 0;
      res = compare(v1, v2);
      if (caml_compare_unordered && !total) return UNORDERED;
      if (res != 0) return res;
      break;
    }
    default: {
      mlsize_t sz1 = Wosize_val(v1);
      mlsize_t sz2 = Wosize_val(v2);
      /* Compare sizes first for speed */
      if (sz1 != sz2) return sz1 - sz2;
      if (sz1 == 0) break;
      /* Remember that we still have to compare fields 1 ... sz - 1 */
      if (sz1 > 1) {
        sp++;
        if (sp >= stk->limit) sp = compare_resize_stack(stk, sp);
        sp->v1 = &Field(v1, 1);
        sp->v2 = &Field(v2, 1);
        sp->count = sz1 - 1;
      }
      /* Continue comparison with first field */
      v1 = Field(v1, 0);
      v2 = Field(v2, 0);
      continue;
    }
    }
  next_item:
    /* Pop one more item to compare, if any */
    if (sp == stk->stack) return EQUAL; /* we're done */
    v1 = *((sp->v1)++);
    v2 = *((sp->v2)++);
    if (--(sp->count) == 0) sp--;
  }
}

CAMLprim value caml_compare(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 1);
  /* Free stack if needed */
  if (res < 0)
    return Val_int(LESS);
  else if (res > 0)
    return Val_int(GREATER);
  else
    return Val_int(EQUAL);
}

CAMLprim value caml_equal(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res == 0);
}

CAMLprim value caml_notequal(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res != 0);
}

CAMLprim value caml_lessthan(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res < 0 && res != UNORDERED);
}

CAMLprim value caml_lessequal(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res <= 0 && res != UNORDERED);
}

CAMLprim value caml_greaterthan(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res > 0);
}

CAMLprim value caml_greaterequal(value v1, value v2)
{
  intnat res = compare_val(v1, v2, 0);
  return Val_int(res >= 0);
}
