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

/* Structured input, compact format */

/* The interface of this file is "caml/intext.h" */

#include <string.h>
#include <stdio.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/intext.h"
#include "caml/io.h"
#include "caml/md5.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/misc.h"
#include "caml/reverse.h"

static unsigned char * intern_src;
/* Reading pointer in block holding input data. */

static unsigned char * intern_input = NULL;
/* Pointer to beginning of block holding input data,
   if non-NULL this pointer will be freed by the cleanup function. */

static header_t * intern_dest;
/* Writing pointer in destination block */

static char * intern_extra_block = NULL;
/* If non-NULL, point to new heap chunk allocated with caml_alloc_for_heap. */

static asize_t obj_counter;
/* Count how many objects seen so far */

static value * intern_obj_table = NULL;
/* The pointers to objects already seen */

static unsigned int intern_color;
/* Color to assign to newly created headers */

static header_t intern_header;
/* Original header of the destination block.
   Meaningful only if intern_extra_block is NULL. */

static value intern_block = 0;
/* Point to the heap block allocated as destination block.
   Meaningful only if intern_extra_block is NULL. */

static char * intern_resolve_code_pointer(unsigned char digest[16],
                                          asize_t offset);

CAMLnoreturn_start
static void intern_bad_code_pointer(unsigned char digest[16])
CAMLnoreturn_end;

static void intern_free_stack(void);

static inline unsigned char read8u(void)
{ return *intern_src++; }

static inline signed char read8s(void)
{ return *intern_src++; }

static inline uint16_t read16u(void)
{
  uint16_t res = (intern_src[0] << 8) + intern_src[1];
  intern_src += 2;
  return res;
}

static inline int16_t read16s(void)
{
  int16_t res = (intern_src[0] << 8) + intern_src[1];
  intern_src += 2;
  return res;
}

static inline uint32_t read32u(void)
{
  uint32_t res =
    ((uint32_t)(intern_src[0]) << 24) + (intern_src[1] << 16)
    + (intern_src[2] << 8) + intern_src[3];
  intern_src += 4;
  return res;
}

static inline int32_t read32s(void)
{
  int32_t res =
    ((uint32_t)(intern_src[0]) << 24) + (intern_src[1] << 16)
    + (intern_src[2] << 8) + intern_src[3];
  intern_src += 4;
  return res;
}

#ifdef ARCH_SIXTYFOUR
static uintnat read64u(void)
{
  uintnat res =
    ((uintnat) (intern_src[0]) << 56)
    + ((uintnat) (intern_src[1]) << 48)
    + ((uintnat) (intern_src[2]) << 40)
    + ((uintnat) (intern_src[3]) << 32)
    + ((uintnat) (intern_src[4]) << 24)
    + ((uintnat) (intern_src[5]) << 16)
    + ((uintnat) (intern_src[6]) << 8)
    + (uintnat) (intern_src[7]);
  intern_src += 8;
  return res;
}
#endif

static inline void readblock(void * dest, intnat len)
{
  memcpy(dest, intern_src, len);
  intern_src += len;
}

static void intern_init(void * src, void * input)
{
  /* This is asserted at the beginning of demarshaling primitives.
     If it fails, it probably means that an exception was raised
     without calling intern_cleanup() during the previous demarshaling. */
  CAMLassert (intern_input == NULL && intern_obj_table == NULL \
     && intern_extra_block == NULL && intern_block == 0);
  intern_src = src;
  intern_input = input;
}

static void intern_cleanup(void)
{
  if (intern_input != NULL) {
     caml_stat_free(intern_input);
     intern_input = NULL;
  }
  if (intern_obj_table != NULL) {
    caml_stat_free(intern_obj_table);
    intern_obj_table = NULL;
  }
  if (intern_extra_block != NULL) {
    /* free newly allocated heap chunk */
    caml_free_for_heap(intern_extra_block);
    intern_extra_block = NULL;
  } else if (intern_block != 0) {
    /* restore original header for heap block, otherwise GC is confused */
    Hd_val(intern_block) = intern_header;
    intern_block = 0;
  }
  /* free the recursion stack */
  intern_free_stack();
}

static void readfloat(double * dest, unsigned int code)
{
  if (sizeof(double) != 8) {
    intern_cleanup();
    caml_invalid_argument("input_value: non-standard floats");
  }
  readblock((char *) dest, 8);
  /* Fix up endianness, if needed */
#if ARCH_FLOAT_ENDIANNESS == 0x76543210
  /* Host is big-endian; fix up if data read is little-endian */
  if (code != CODE_DOUBLE_BIG) Reverse_64(dest, dest);
#elif ARCH_FLOAT_ENDIANNESS == 0x01234567
  /* Host is little-endian; fix up if data read is big-endian */
  if (code != CODE_DOUBLE_LITTLE) Reverse_64(dest, dest);
#else
  /* Host is neither big nor little; permute as appropriate */
  if (code == CODE_DOUBLE_LITTLE)
    Permute_64(dest, ARCH_FLOAT_ENDIANNESS, dest, 0x01234567)
  else
    Permute_64(dest, ARCH_FLOAT_ENDIANNESS, dest, 0x76543210);
#endif
}

/* [len] is a number of floats */
static void readfloats(double * dest, mlsize_t len, unsigned int code)
{
  mlsize_t i;
  if (sizeof(double) != 8) {
    intern_cleanup();
    caml_invalid_argument("input_value: non-standard floats");
  }
  readblock((char *) dest, len * 8);
  /* Fix up endianness, if needed */
#if ARCH_FLOAT_ENDIANNESS == 0x76543210
  /* Host is big-endian; fix up if data read is little-endian */
  if (code != CODE_DOUBLE_ARRAY8_BIG &&
      code != CODE_DOUBLE_ARRAY32_BIG) {
    for (i = 0; i < len; i++) Reverse_64(dest + i, dest + i);
  }
#elif ARCH_FLOAT_ENDIANNESS == 0x01234567
  /* Host is little-endian; fix up if data read is big-endian */
  if (code != CODE_DOUBLE_ARRAY8_LITTLE &&
      code != CODE_DOUBLE_ARRAY32_LITTLE) {
    for (i = 0; i < len; i++) Reverse_64(dest + i, dest + i);
  }
#else
  /* Host is neither big nor little; permute as appropriate */
  if (code == CODE_DOUBLE_ARRAY8_LITTLE ||
      code == CODE_DOUBLE_ARRAY32_LITTLE) {
    for (i = 0; i < len; i++)
      Permute_64(dest + i, ARCH_FLOAT_ENDIANNESS, dest + i, 0x01234567);
  } else {
    for (i = 0; i < len; i++)
      Permute_64(dest + i, ARCH_FLOAT_ENDIANNESS, dest + i, 0x76543210);
  }
#endif
}

/* Item on the stack with defined operation */
struct intern_item {
  value * dest;
  intnat arg;
  enum {
    OReadItems, /* read arg items and store them in dest[0], dest[1], ... */
    OFreshOID,  /* generate a fresh OID and store it in *dest */
    OShift      /* offset *dest by arg */
  } op;
};

/* FIXME: This is duplicated in two other places, with the only difference of
   the type of elements stored in the stack. Possible solution in C would
   be to instantiate stack these function via. C preprocessor macro.
 */

#define INTERN_STACK_INIT_SIZE 256
#define INTERN_STACK_MAX_SIZE (1024*1024*100)

static struct intern_item intern_stack_init[INTERN_STACK_INIT_SIZE];

static struct intern_item * intern_stack = intern_stack_init;
static struct intern_item * intern_stack_limit = intern_stack_init
                                                   + INTERN_STACK_INIT_SIZE;

/* Free the recursion stack if needed */
static void intern_free_stack(void)
{
  if (intern_stack != intern_stack_init) {
    caml_stat_free(intern_stack);
    /* Reinitialize the globals for next time around */
    intern_stack = intern_stack_init;
    intern_stack_limit = intern_stack + INTERN_STACK_INIT_SIZE;
  }
}

/* Same, then raise Out_of_memory */
static void intern_stack_overflow(void)
{
  caml_gc_message (0x04, "Stack overflow in un-marshaling value\n");
  intern_free_stack();
  caml_raise_out_of_memory();
}

static struct intern_item * intern_resize_stack(struct intern_item * sp)
{
  asize_t newsize = 2 * (intern_stack_limit - intern_stack);
  asize_t sp_offset = sp - intern_stack;
  struct intern_item * newstack;

  if (newsize >= INTERN_STACK_MAX_SIZE) intern_stack_overflow();
  if (intern_stack == intern_stack_init) {
    newstack = caml_stat_alloc_noexc(sizeof(struct intern_item) * newsize);
    if (newstack == NULL) intern_stack_overflow();
    memcpy(newstack, intern_stack_init,
           sizeof(struct intern_item) * INTERN_STACK_INIT_SIZE);
  } else {
    newstack = caml_stat_resize_noexc(intern_stack,
                                      sizeof(struct intern_item) * newsize);
    if (newstack == NULL) intern_stack_overflow();
  }
  intern_stack = newstack;
  intern_stack_limit = newstack + newsize;
  return newstack + sp_offset;
}

/* Convenience macros for requesting operation on the stack */
#define PushItem()                                                      \
  do {                                                                  \
    sp++;                                                               \
    if (sp >= intern_stack_limit) sp = intern_resize_stack(sp);         \
  } while(0)

#define ReadItems(_dest,_n)                                             \
  do {                                                                  \
    if (_n > 0) {                                                       \
      PushItem();                                                       \
      sp->op = OReadItems;                                              \
      sp->dest = _dest;                                                 \
      sp->arg = _n;                                                     \
    }                                                                   \
  } while(0)

static void intern_rec(value *dest)
{
  unsigned int code;
  tag_t tag;
  mlsize_t size, len, ofs_ind;
  value v;
  asize_t ofs;
  header_t header;
  unsigned char digest[16];
  struct custom_operations * ops;
  char * codeptr;
  struct intern_item * sp;

  sp = intern_stack;

  /* Initially let's try to read the first object from the stream */
  ReadItems(dest, 1);

  /* The un-marshaler loop, the recursion is unrolled */
  while(sp != intern_stack) {

  /* Interpret next item on the stack */
  dest = sp->dest;
  switch (sp->op) {
  case OFreshOID:
    /* Refresh the object ID */
    /* but do not do it for predefined exception slots */
    if (Long_val(Field((value)dest, 1)) >= 0)
      caml_set_oo_id((value)dest);
    /* Pop item and iterate */
    sp--;
    break;
  case OShift:
    /* Shift value by an offset */
    *dest += sp->arg;
    /* Pop item and iterate */
    sp--;
    break;
  case OReadItems:
    /* Pop item */
    sp->dest++;
    if (--(sp->arg) == 0) sp--;
    /* Read a value and set v to this value */
  code = read8u();
  if (code >= PREFIX_SMALL_INT) {
    if (code >= PREFIX_SMALL_BLOCK) {
      /* Small block */
      tag = code & 0xF;
      size = (code >> 4) & 0x7;
    read_block:
      if (size == 0) {
        v = Atom(tag);
      } else {
        v = Val_hp(intern_dest);
        if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
        *intern_dest = Make_header_allocated_here(size, tag, intern_color);
        intern_dest += 1 + size;
        /* For objects, we need to freshen the oid */
        if (tag == Object_tag) {
          CAMLassert(size >= 2);
          /* Request to read rest of the elements of the block */
          ReadItems(&Field(v, 2), size - 2);
          /* Request freshing OID */
          PushItem();
          sp->op = OFreshOID;
          sp->dest = (value*) v;
          sp->arg = 1;
          /* Finally read first two block elements: method table and old OID */
          ReadItems(&Field(v, 0), 2);
        } else
          /* If it's not an object then read the contents of the block */
          ReadItems(&Field(v, 0), size);
      }
    } else {
      /* Small integer */
      v = Val_int(code & 0x3F);
    }
  } else {
    if (code >= PREFIX_SMALL_STRING) {
      /* Small string */
      len = (code & 0x1F);
    read_string:
      size = (len + sizeof(value)) / sizeof(value);
      v = Val_hp(intern_dest);
      if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
      *intern_dest = Make_header_allocated_here(size, String_tag, intern_color);
      intern_dest += 1 + size;
      Field(v, size - 1) = 0;
      ofs_ind = Bsize_wsize(size) - 1;
      Byte(v, ofs_ind) = ofs_ind - len;
      readblock((char *)String_val(v), len);
    } else {
      switch(code) {
      case CODE_INT8:
        v = Val_long(read8s());
        break;
      case CODE_INT16:
        v = Val_long(read16s());
        break;
      case CODE_INT32:
        v = Val_long(read32s());
        break;
      case CODE_INT64:
#ifdef ARCH_SIXTYFOUR
        v = Val_long((intnat) (read64u()));
        break;
#else
        intern_cleanup();
        caml_failwith("input_value: integer too large");
        break;
#endif
      case CODE_SHARED8:
        ofs = read8u();
      read_shared:
        CAMLassert (ofs > 0);
        CAMLassert (ofs <= obj_counter);
        CAMLassert (intern_obj_table != NULL);
        v = intern_obj_table[obj_counter - ofs];
        break;
      case CODE_SHARED16:
        ofs = read16u();
        goto read_shared;
      case CODE_SHARED32:
        ofs = read32u();
        goto read_shared;
#ifdef ARCH_SIXTYFOUR
      case CODE_SHARED64:
        ofs = read64u();
        goto read_shared;
#endif
      case CODE_BLOCK32:
        header = (header_t) read32u();
        tag = Tag_hd(header);
        size = Wosize_hd(header);
        goto read_block;
#ifdef ARCH_SIXTYFOUR
      case CODE_BLOCK64:
        header = (header_t) read64u();
        tag = Tag_hd(header);
        size = Wosize_hd(header);
        goto read_block;
#endif
      case CODE_STRING8:
        len = read8u();
        goto read_string;
      case CODE_STRING32:
        len = read32u();
        goto read_string;
#ifdef ARCH_SIXTYFOUR
      case CODE_STRING64:
        len = read64u();
        goto read_string;
#endif
      case CODE_DOUBLE_LITTLE:
      case CODE_DOUBLE_BIG:
        v = Val_hp(intern_dest);
        if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
        *intern_dest = Make_header_allocated_here(Double_wosize, Double_tag,
                                                  intern_color);
        intern_dest += 1 + Double_wosize;
        readfloat((double *) v, code);
        break;
      case CODE_DOUBLE_ARRAY8_LITTLE:
      case CODE_DOUBLE_ARRAY8_BIG:
        len = read8u();
      read_double_array:
        size = len * Double_wosize;
        v = Val_hp(intern_dest);
        if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
        *intern_dest = Make_header_allocated_here(size, Double_array_tag,
                                                  intern_color);
        intern_dest += 1 + size;
        readfloats((double *) v, len, code);
        break;
      case CODE_DOUBLE_ARRAY32_LITTLE:
      case CODE_DOUBLE_ARRAY32_BIG:
        len = read32u();
        goto read_double_array;
#ifdef ARCH_SIXTYFOUR
      case CODE_DOUBLE_ARRAY64_LITTLE:
      case CODE_DOUBLE_ARRAY64_BIG:
        len = read64u();
        goto read_double_array;
#endif
      case CODE_CODEPOINTER:
        ofs = read32u();
        readblock(digest, 16);
        codeptr = intern_resolve_code_pointer(digest, ofs);
        if (codeptr != NULL) {
          v = (value) codeptr;
        } else {
          value * function_placeholder =
            caml_named_value ("Debugger.function_placeholder");
          if (function_placeholder != NULL) {
            v = *function_placeholder;
          } else {
            intern_cleanup();
            intern_bad_code_pointer(digest);
          }
        }
        break;
      case CODE_INFIXPOINTER:
        ofs = read32u();
        /* Read a value to *dest, then offset *dest by ofs */
        PushItem();
        sp->dest = dest;
        sp->op = OShift;
        sp->arg = ofs;
        ReadItems(dest, 1);
        continue;  /* with next iteration of main loop, skipping *dest = v */
      case CODE_CUSTOM:
        ops = caml_find_custom_operations((char *) intern_src);
        if (ops == NULL) {
          intern_cleanup();
          caml_failwith("input_value: unknown custom block identifier");
        }
        while (*intern_src++ != 0) /*nothing*/;  /*skip identifier*/
        size = ops->deserialize((void *) (intern_dest + 2));
        size = 1 + (size + sizeof(value) - 1) / sizeof(value);
        v = Val_hp(intern_dest);
        if (intern_obj_table != NULL) intern_obj_table[obj_counter++] = v;
        *intern_dest = Make_header_allocated_here(size, Custom_tag,
                                                  intern_color);
        Custom_ops_val(v) = ops;

        if (ops->finalize != NULL && Is_young(v)) {
          /* Remember that the block has a finalizer. */
          add_to_custom_table (&caml_custom_table, v, 0, 1);
        }

        intern_dest += 1 + size;
        break;
      default:
        intern_cleanup();
        caml_failwith("input_value: ill-formed message");
      }
    }
  }
  /* end of case OReadItems */
  *dest = v;
  break;
  default:
    CAMLassert(0);
  }
  }
  /* We are done. Cleanup the stack and leave the function */
  intern_free_stack();
}

static void intern_alloc(mlsize_t whsize, mlsize_t num_objects,
      int outside_heap)
{
  mlsize_t wosize;

  if (whsize == 0) {
    CAMLassert (intern_extra_block == NULL && intern_block == 0
         && intern_obj_table == NULL);
    return;
  }
  wosize = Wosize_whsize(whsize);
  if (outside_heap || wosize > Max_wosize) {
    /* Round desired size up to next page */
    asize_t request =
      ((Bsize_wsize(whsize) + Page_size - 1) >> Page_log) << Page_log;
    intern_extra_block = caml_alloc_for_heap(request);
    if (intern_extra_block == NULL) {
      intern_cleanup();
      caml_raise_out_of_memory();
    }
    intern_color =
      outside_heap ? Caml_black : caml_allocation_color(intern_extra_block);
    intern_dest = (header_t *) intern_extra_block;
    CAMLassert (intern_block == 0);
  } else {
    /* this is a specialised version of caml_alloc from alloc.c */
    if (wosize <= Max_young_wosize){
      if (wosize == 0){
        intern_block = Atom (String_tag);
      } else {
        intern_block = caml_alloc_small (wosize, String_tag);
      }
    }else{
      intern_block = caml_alloc_shr_no_raise (wosize, String_tag);
      /* do not do the urgent_gc check here because it might darken
         intern_block into gray and break the intern_color assertion below */
      if (intern_block == 0) {
        intern_cleanup();
        caml_raise_out_of_memory();
      }
    }
    intern_header = Hd_val(intern_block);
    intern_color = Color_hd(intern_header);
    CAMLassert (intern_color == Caml_white || intern_color == Caml_black);
    intern_dest = (header_t *) Hp_val(intern_block);
    CAMLassert (intern_extra_block == NULL);
  }
  obj_counter = 0;
  if (num_objects > 0) {
    intern_obj_table = (value *) caml_stat_alloc_noexc(num_objects * sizeof(value));
    if (intern_obj_table == NULL) {
      intern_cleanup();
      caml_raise_out_of_memory();
    }
  } else
    CAMLassert(intern_obj_table == NULL);
}

static void intern_add_to_heap(mlsize_t whsize)
{
  /* Add new heap chunk to heap if needed */
  if (intern_extra_block != NULL) {
    /* If heap chunk not filled totally, build free block at end */
    asize_t request = Chunk_size (intern_extra_block);
    header_t * end_extra_block =
      (header_t *) intern_extra_block + Wsize_bsize(request);
    CAMLassert(intern_block == 0);
    CAMLassert(intern_dest <= end_extra_block);
    if (intern_dest < end_extra_block){
      caml_make_free_blocks ((value *) intern_dest,
                             end_extra_block - intern_dest, 0, Caml_white);
    }
    caml_allocated_words +=
      Wsize_bsize ((char *) intern_dest - intern_extra_block);
    caml_add_to_heap(intern_extra_block);
    intern_extra_block = NULL; // To prevent intern_cleanup freeing it
  } else {
    intern_block = 0; // To prevent intern_cleanup rewriting its header
  }
}

/* Parsing the header */

struct marshal_header {
  uint32_t magic;
  int header_len;
  uintnat data_len;
  uintnat num_objects;
  uintnat whsize;
};

static void caml_parse_header(char * fun_name,
                              /*out*/ struct marshal_header * h)
{
  char errmsg[100];

  h->magic = read32u();
  switch(h->magic) {
  case Intext_magic_number_small:
    h->header_len = 20;
    h->data_len = read32u();
    h->num_objects = read32u();
#ifdef ARCH_SIXTYFOUR
    read32u();
    h->whsize = read32u();
#else
    h->whsize = read32u();
    read32u();
#endif
    break;
  case Intext_magic_number_big:
#ifdef ARCH_SIXTYFOUR
    h->header_len = 32;
    read32u();
    h->data_len = read64u();
    h->num_objects = read64u();
    h->whsize = read64u();
#else
    errmsg[sizeof(errmsg) - 1] = 0;
    snprintf(errmsg, sizeof(errmsg) - 1,
             "%s: object too large to be read back on a 32-bit platform",
             fun_name);
    caml_failwith(errmsg);
#endif
    break;
  default:
    errmsg[sizeof(errmsg) - 1] = 0;
    snprintf(errmsg, sizeof(errmsg) - 1,
             "%s: bad object",
             fun_name);
    caml_failwith(errmsg);
  }
}

/* Reading from a channel */

static value caml_input_val_core(struct channel *chan, int outside_heap)
{
  intnat r;
  char header[32];
  struct marshal_header h;
  char * block;
  value res;

  if (! caml_channel_binary_mode(chan))
    caml_failwith("input_value: not a binary channel");
  /* Read and parse the header */
  r = caml_really_getblock(chan, header, 20);
  if (r == 0)
    caml_raise_end_of_file();
  else if (r < 20)
    caml_failwith("input_value: truncated object");
  intern_src = (unsigned char *) header;
  if (read32u() == Intext_magic_number_big) {
    /* Finish reading the header */
    if (caml_really_getblock(chan, header + 20, 32 - 20) < 32 - 20)
      caml_failwith("input_value: truncated object");
  }
  intern_src = (unsigned char *) header;
  caml_parse_header("input_value", &h);
  /* Read block from channel */
  block = caml_stat_alloc(h.data_len);
  /* During [caml_really_getblock], concurrent [caml_input_val] operations
     can take place (via signal handlers or context switching in systhreads),
     and [intern_input] may change.  So, wait until [caml_really_getblock]
     is over before using [intern_input] and the other global vars. */
  if (caml_really_getblock(chan, block, h.data_len) < h.data_len) {
    caml_stat_free(block);
    caml_failwith("input_value: truncated object");
  }
  /* Initialize global state */
  intern_init(block, block);
  intern_alloc(h.whsize, h.num_objects, outside_heap);
  /* Fill it in */
  intern_rec(&res);
  if (!outside_heap) {
    intern_add_to_heap(h.whsize);
  } else {
    caml_disown_for_heap(intern_extra_block);
    intern_extra_block = NULL;
    intern_block = 0;
  }
  /* Free everything */
  intern_cleanup();
  return caml_check_urgent_gc(res);
}

value caml_input_val(struct channel* chan)
{
  return caml_input_val_core(chan, 0);
}

CAMLprim value caml_input_value(value vchan)
{
  CAMLparam1 (vchan);
  struct channel * chan = Channel(vchan);
  CAMLlocal1 (res);

  Lock(chan);
  res = caml_input_val(chan);
  Unlock(chan);
  CAMLreturn (res);
}

/* Reading from memory-resident blocks */

CAMLprim value caml_input_value_to_outside_heap(value vchan)
{
  CAMLparam1 (vchan);
  struct channel * chan = Channel(vchan);
  CAMLlocal1 (res);

  Lock(chan);
  res = caml_input_val_core(chan, 1);
  Unlock(chan);
  CAMLreturn (res);
}

CAMLexport value caml_input_val_from_string(value str, intnat ofs)
{
  CAMLparam1 (str);
  CAMLlocal1 (obj);
  struct marshal_header h;

  /* Initialize global state */
  intern_init(&Byte_u(str, ofs), NULL);
  caml_parse_header("input_val_from_string", &h);
  if (ofs + h.header_len + h.data_len > caml_string_length(str))
    caml_failwith("input_val_from_string: bad length");
  /* Allocate result */
  intern_alloc(h.whsize, h.num_objects, 0);
  intern_src = &Byte_u(str, ofs + h.header_len); /* If a GC occurred */
  /* Fill it in */
  intern_rec(&obj);
  intern_add_to_heap(h.whsize);
  /* Free everything */
  intern_cleanup();
  CAMLreturn (caml_check_urgent_gc(obj));
}

CAMLprim value caml_input_value_from_string(value str, value ofs)
{
  return caml_input_val_from_string(str, Long_val(ofs));
}

static value input_val_from_block(struct marshal_header * h)
{
  value obj;
  /* Allocate result */
  intern_alloc(h->whsize, h->num_objects, 0);
  /* Fill it in */
  intern_rec(&obj);
  intern_add_to_heap(h->whsize);
  /* Free internal data structures */
  intern_cleanup();
  return caml_check_urgent_gc(obj);
}

CAMLexport value caml_input_value_from_malloc(char * data, intnat ofs)
{
  struct marshal_header h;

  intern_init(data + ofs, data);

  caml_parse_header("input_value_from_malloc", &h);

  return input_val_from_block(&h);
}

/* [len] is a number of bytes */
CAMLexport value caml_input_value_from_block(char * data, intnat len)
{
  struct marshal_header h;

  /* Initialize global state */
  intern_init(data, NULL);
  caml_parse_header("input_value_from_block", &h);
  if (h.header_len + h.data_len > len)
    caml_failwith("input_val_from_block: bad length");
  return input_val_from_block(&h);
}

/* [ofs] is a [value] that represents a number of bytes
   result is a [value] that represents a number of bytes
   To handle both the small and the big format,
   we assume 20 bytes are available at [buff + ofs],
   and we return the data size + the length of the part of the header
   that remains to be read. */

CAMLprim value caml_marshal_data_size(value buff, value ofs)
{
  uint32_t magic;
  int header_len;
  uintnat data_len;

  intern_src = &Byte_u(buff, Long_val(ofs));
  magic = read32u();
  switch(magic) {
  case Intext_magic_number_small:
    header_len = 20;
    data_len = read32u();
    break;
  case Intext_magic_number_big:
#ifdef ARCH_SIXTYFOUR
    header_len = 32;
    read32u();
    data_len = read64u();
#else
    caml_failwith("Marshal.data_size: "
                  "object too large to be read back on a 32-bit platform");
#endif
    break;
  default:
    caml_failwith("Marshal.data_size: bad object");
  }
  return Val_long((header_len - 20) + data_len);
}

/* Resolution of code pointers */

static char * intern_resolve_code_pointer(unsigned char digest[16],
                                          asize_t offset)
{
  int i;
  for (i = caml_code_fragments_table.size - 1; i >= 0; i--) {
    struct code_fragment * cf = caml_code_fragments_table.contents[i];
    if (! cf->digest_computed) {
      caml_md5_block(cf->digest, cf->code_start, cf->code_end - cf->code_start);
      cf->digest_computed = 1;
    }
    if (memcmp(digest, cf->digest, 16) == 0) {
      if (cf->code_start + offset < cf->code_end)
        return cf->code_start + offset;
      else
        return NULL;
    }
  }
  return NULL;
}

static void intern_bad_code_pointer(unsigned char digest[16])
{
  char msg[256];
  snprintf(msg, sizeof(msg),
               "input_value: unknown code module "
               "%02X%02X%02X%02X%02X%02X%02X%02X"
               "%02X%02X%02X%02X%02X%02X%02X%02X",
          digest[0], digest[1], digest[2], digest[3],
          digest[4], digest[5], digest[6], digest[7],
          digest[8], digest[9], digest[10], digest[11],
          digest[12], digest[13], digest[14], digest[15]);
  caml_failwith(msg);
}

/* Functions for writing user-defined marshallers */

CAMLexport int caml_deserialize_uint_1(void)
{
  return read8u();
}

CAMLexport int caml_deserialize_sint_1(void)
{
  return read8s();
}

CAMLexport int caml_deserialize_uint_2(void)
{
  return read16u();
}

CAMLexport int caml_deserialize_sint_2(void)
{
  return read16s();
}

CAMLexport uint32_t caml_deserialize_uint_4(void)
{
  return read32u();
}

CAMLexport int32_t caml_deserialize_sint_4(void)
{
  return read32s();
}

CAMLexport uint64_t caml_deserialize_uint_8(void)
{
  uint64_t i;
  caml_deserialize_block_8(&i, 1);
  return i;
}

CAMLexport int64_t caml_deserialize_sint_8(void)
{
  int64_t i;
  caml_deserialize_block_8(&i, 1);
  return i;
}

CAMLexport float caml_deserialize_float_4(void)
{
  float f;
  caml_deserialize_block_4(&f, 1);
  return f;
}

CAMLexport double caml_deserialize_float_8(void)
{
  double f;
  caml_deserialize_block_float_8(&f, 1);
  return f;
}

CAMLexport void caml_deserialize_block_1(void * data, intnat len)
{
  memcpy(data, intern_src, len);
  intern_src += len;
}

CAMLexport void caml_deserialize_block_2(void * data, intnat len)
{
#ifndef ARCH_BIG_ENDIAN
  unsigned char * p, * q;
  for (p = intern_src, q = data; len > 0; len--, p += 2, q += 2)
    Reverse_16(q, p);
  intern_src = p;
#else
  memcpy(data, intern_src, len * 2);
  intern_src += len * 2;
#endif
}

CAMLexport void caml_deserialize_block_4(void * data, intnat len)
{
#ifndef ARCH_BIG_ENDIAN
  unsigned char * p, * q;
  for (p = intern_src, q = data; len > 0; len--, p += 4, q += 4)
    Reverse_32(q, p);
  intern_src = p;
#else
  memcpy(data, intern_src, len * 4);
  intern_src += len * 4;
#endif
}

CAMLexport void caml_deserialize_block_8(void * data, intnat len)
{
#ifndef ARCH_BIG_ENDIAN
  unsigned char * p, * q;
  for (p = intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Reverse_64(q, p);
  intern_src = p;
#else
  memcpy(data, intern_src, len * 8);
  intern_src += len * 8;
#endif
}

CAMLexport void caml_deserialize_block_float_8(void * data, intnat len)
{
#if ARCH_FLOAT_ENDIANNESS == 0x01234567
  memcpy(data, intern_src, len * 8);
  intern_src += len * 8;
#elif ARCH_FLOAT_ENDIANNESS == 0x76543210
  unsigned char * p, * q;
  for (p = intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Reverse_64(q, p);
  intern_src = p;
#else
  unsigned char * p, * q;
  for (p = intern_src, q = data; len > 0; len--, p += 8, q += 8)
    Permute_64(q, ARCH_FLOAT_ENDIANNESS, p, 0x01234567);
  intern_src = p;
#endif
}

CAMLexport void caml_deserialize_error(char * msg)
{
  intern_cleanup();
  caml_failwith(msg);
}
