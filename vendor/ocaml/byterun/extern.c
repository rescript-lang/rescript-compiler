/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Structured output */

/* The interface of this file is "caml/intext.h" */

#include <string.h>
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/intext.h"
#include "caml/io.h"
#include "caml/md5.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/reverse.h"

static uintnat obj_counter;  /* Number of objects emitted so far */
static uintnat size_32;  /* Size in words of 32-bit block for struct. */
static uintnat size_64;  /* Size in words of 64-bit block for struct. */

/* Flags affecting marshaling */

enum {
  NO_SHARING = 1,               /* Flag to ignore sharing */
  CLOSURES = 2,                 /* Flag to allow marshaling code pointers */
  COMPAT_32 = 4                 /* Flag to ensure that output can safely
                                   be read back on a 32-bit platform */
};

static int extern_flags;        /* logical or of some of the flags above */

/* Trail mechanism to undo forwarding pointers put inside objects */

struct trail_entry {
  value obj;    /* address of object + initial color in low 2 bits */
  value field0; /* initial contents of field 0 */
};

struct trail_block {
  struct trail_block * previous;
  struct trail_entry entries[ENTRIES_PER_TRAIL_BLOCK];
};

static struct trail_block extern_trail_first;
static struct trail_block * extern_trail_block;
static struct trail_entry * extern_trail_cur, * extern_trail_limit;


/* Stack for pending values to marshal */

struct extern_item { value * v; mlsize_t count; };

#define EXTERN_STACK_INIT_SIZE 256
#define EXTERN_STACK_MAX_SIZE (1024*1024*100)

static struct extern_item extern_stack_init[EXTERN_STACK_INIT_SIZE];

static struct extern_item * extern_stack = extern_stack_init;
static struct extern_item * extern_stack_limit = extern_stack_init
                                                   + EXTERN_STACK_INIT_SIZE;

/* Forward declarations */

static void extern_out_of_memory(void);
static void extern_invalid_argument(char *msg);
static void extern_failwith(char *msg);
static void extern_stack_overflow(void);
static struct code_fragment * extern_find_code(char *addr);
static void extern_replay_trail(void);
static void free_extern_output(void);

/* Free the extern stack if needed */
static void extern_free_stack(void)
{
  if (extern_stack != extern_stack_init) {
    free(extern_stack);
    /* Reinitialize the globals for next time around */
    extern_stack = extern_stack_init;
    extern_stack_limit = extern_stack + EXTERN_STACK_INIT_SIZE;
  }
}

static struct extern_item * extern_resize_stack(struct extern_item * sp)
{
  asize_t newsize = 2 * (extern_stack_limit - extern_stack);
  asize_t sp_offset = sp - extern_stack;
  struct extern_item * newstack;

  if (newsize >= EXTERN_STACK_MAX_SIZE) extern_stack_overflow();
  if (extern_stack == extern_stack_init) {
    newstack = malloc(sizeof(struct extern_item) * newsize);
    if (newstack == NULL) extern_stack_overflow();
    memcpy(newstack, extern_stack_init,
           sizeof(struct extern_item) * EXTERN_STACK_INIT_SIZE);
  } else {
    newstack =
      realloc(extern_stack, sizeof(struct extern_item) * newsize);
    if (newstack == NULL) extern_stack_overflow();
  }
  extern_stack = newstack;
  extern_stack_limit = newstack + newsize;
  return newstack + sp_offset;
}

/* Initialize the trail */

static void init_extern_trail(void)
{
  extern_trail_block = &extern_trail_first;
  extern_trail_cur = extern_trail_block->entries;
  extern_trail_limit = extern_trail_block->entries + ENTRIES_PER_TRAIL_BLOCK;
}

/* Replay the trail, undoing the in-place modifications
   performed on objects */

static void extern_replay_trail(void)
{
  struct trail_block * blk, * prevblk;
  struct trail_entry * ent, * lim;

  blk = extern_trail_block;
  lim = extern_trail_cur;
  while (1) {
    for (ent = &(blk->entries[0]); ent < lim; ent++) {
      value obj = ent->obj;
      color_t colornum = obj & 3;
      obj = obj & ~3;
      Hd_val(obj) = Coloredhd_hd(Hd_val(obj), colornum);
      Field(obj, 0) = ent->field0;
    }
    if (blk == &extern_trail_first) break;
    prevblk = blk->previous;
    free(blk);
    blk = prevblk;
    lim = &(blk->entries[ENTRIES_PER_TRAIL_BLOCK]);
  }
  /* Protect against a second call to extern_replay_trail */
  extern_trail_block = &extern_trail_first;
  extern_trail_cur = extern_trail_block->entries;
}

/* Set forwarding pointer on an object and add corresponding entry
   to the trail. */

static void extern_record_location(value obj)
{
  header_t hdr;

  if (extern_flags & NO_SHARING) return;
  if (extern_trail_cur == extern_trail_limit) {
    struct trail_block * new_block = malloc(sizeof(struct trail_block));
    if (new_block == NULL) extern_out_of_memory();
    new_block->previous = extern_trail_block;
    extern_trail_block = new_block;
    extern_trail_cur = extern_trail_block->entries;
    extern_trail_limit = extern_trail_block->entries + ENTRIES_PER_TRAIL_BLOCK;
  }
  hdr = Hd_val(obj);
  extern_trail_cur->obj = obj | Colornum_hd(hdr);
  extern_trail_cur->field0 = Field(obj, 0);
  extern_trail_cur++;
  Hd_val(obj) = Bluehd_hd(hdr);
  Field(obj, 0) = (value) obj_counter;
  obj_counter++;
}

/* To buffer the output */

static char * extern_userprovided_output;
static char * extern_ptr, * extern_limit;

struct output_block {
  struct output_block * next;
  char * end;
  char data[SIZE_EXTERN_OUTPUT_BLOCK];
};

static struct output_block * extern_output_first, * extern_output_block;

static void init_extern_output(void)
{
  extern_userprovided_output = NULL;
  extern_output_first = malloc(sizeof(struct output_block));
  if (extern_output_first == NULL) caml_raise_out_of_memory();
  extern_output_block = extern_output_first;
  extern_output_block->next = NULL;
  extern_ptr = extern_output_block->data;
  extern_limit = extern_output_block->data + SIZE_EXTERN_OUTPUT_BLOCK;
}

static void close_extern_output(void)
{
  if (extern_userprovided_output == NULL){
    extern_output_block->end = extern_ptr;
  }
}

static void free_extern_output(void)
{
  struct output_block * blk, * nextblk;

  if (extern_userprovided_output != NULL) return;
  for (blk = extern_output_first; blk != NULL; blk = nextblk) {
    nextblk = blk->next;
    free(blk);
  }
  extern_output_first = NULL;
  extern_free_stack();
}

static void grow_extern_output(intnat required)
{
  struct output_block * blk;
  intnat extra;

  if (extern_userprovided_output != NULL) {
    extern_failwith("Marshal.to_buffer: buffer overflow");
  }
  extern_output_block->end = extern_ptr;
  if (required <= SIZE_EXTERN_OUTPUT_BLOCK / 2)
    extra = 0;
  else
    extra = required;
  blk = malloc(sizeof(struct output_block) + extra);
  if (blk == NULL) extern_out_of_memory();
  extern_output_block->next = blk;
  extern_output_block = blk;
  extern_output_block->next = NULL;
  extern_ptr = extern_output_block->data;
  extern_limit = extern_output_block->data + SIZE_EXTERN_OUTPUT_BLOCK + extra;
}

static intnat extern_output_length(void)
{
  struct output_block * blk;
  intnat len;

  if (extern_userprovided_output != NULL) {
    return extern_ptr - extern_userprovided_output;
  } else {
    for (len = 0, blk = extern_output_first; blk != NULL; blk = blk->next)
      len += blk->end - blk->data;
    return len;
  }
}

/* Exception raising, with cleanup */

static void extern_out_of_memory(void)
{
  extern_replay_trail();
  free_extern_output();
  caml_raise_out_of_memory();
}

static void extern_invalid_argument(char *msg)
{
  extern_replay_trail();
  free_extern_output();
  caml_invalid_argument(msg);
}

static void extern_failwith(char *msg)
{
  extern_replay_trail();
  free_extern_output();
  caml_failwith(msg);
}

static void extern_stack_overflow(void)
{
  caml_gc_message (0x04, "Stack overflow in marshaling value\n", 0);
  extern_replay_trail();
  free_extern_output();
  caml_raise_out_of_memory();
}

/* Write characters, integers, and blocks in the output buffer */

#define Write(c) \
  if (extern_ptr >= extern_limit) grow_extern_output(1); \
  *extern_ptr++ = (c)

static void writeblock(char *data, intnat len)
{
  if (extern_ptr + len > extern_limit) grow_extern_output(len);
  memmove(extern_ptr, data, len);
  extern_ptr += len;
}

#if ARCH_FLOAT_ENDIANNESS == 0x01234567 || ARCH_FLOAT_ENDIANNESS == 0x76543210
#define writeblock_float8(data,ndoubles) \
  writeblock((char *)(data), (ndoubles) * 8)
#else
#define writeblock_float8(data,ndoubles) \
  caml_serialize_block_float_8((data), (ndoubles))
#endif

static void writecode8(int code, intnat val)
{
  if (extern_ptr + 2 > extern_limit) grow_extern_output(2);
  extern_ptr[0] = code;
  extern_ptr[1] = val;
  extern_ptr += 2;
}

static void writecode16(int code, intnat val)
{
  if (extern_ptr + 3 > extern_limit) grow_extern_output(3);
  extern_ptr[0] = code;
  extern_ptr[1] = val >> 8;
  extern_ptr[2] = val;
  extern_ptr += 3;
}

static void write32(intnat val)
{
  if (extern_ptr + 4 > extern_limit) grow_extern_output(4);
  extern_ptr[0] = val >> 24;
  extern_ptr[1] = val >> 16;
  extern_ptr[2] = val >> 8;
  extern_ptr[3] = val;
  extern_ptr += 4;
}

static void writecode32(int code, intnat val)
{
  if (extern_ptr + 5 > extern_limit) grow_extern_output(5);
  extern_ptr[0] = code;
  extern_ptr[1] = val >> 24;
  extern_ptr[2] = val >> 16;
  extern_ptr[3] = val >> 8;
  extern_ptr[4] = val;
  extern_ptr += 5;
}

#ifdef ARCH_SIXTYFOUR
static void writecode64(int code, intnat val)
{
  int i;
  if (extern_ptr + 9 > extern_limit) grow_extern_output(9);
  *extern_ptr ++ = code;
  for (i = 64 - 8; i >= 0; i -= 8) *extern_ptr++ = val >> i;
}
#endif

/* Marshal the given value in the output buffer */

static void extern_rec(value v)
{
  struct code_fragment * cf;
  struct extern_item * sp;
  sp = extern_stack;

  while(1) {
  if (Is_long(v)) {
    intnat n = Long_val(v);
    if (n >= 0 && n < 0x40) {
      Write(PREFIX_SMALL_INT + n);
    } else if (n >= -(1 << 7) && n < (1 << 7)) {
      writecode8(CODE_INT8, n);
    } else if (n >= -(1 << 15) && n < (1 << 15)) {
      writecode16(CODE_INT16, n);
#ifdef ARCH_SIXTYFOUR
    } else if (n < -((intnat)1 << 30) || n >= ((intnat)1 << 30)) {
      if (extern_flags & COMPAT_32)
        extern_failwith("output_value: integer cannot be read back on "
                        "32-bit platform");
      writecode64(CODE_INT64, n);
#endif
    } else
      writecode32(CODE_INT32, n);
    goto next_item;
  }
  if (Is_in_value_area(v)) {
    header_t hd = Hd_val(v);
    tag_t tag = Tag_hd(hd);
    mlsize_t sz = Wosize_hd(hd);

    if (tag == Forward_tag) {
      value f = Forward_val (v);
      if (Is_block (f)
          && (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
              || Tag_val (f) == Lazy_tag || Tag_val (f) == Double_tag)){
        /* Do not short-circuit the pointer. */
      }else{
        v = f;
        continue;
      }
    }
    /* Atoms are treated specially for two reasons: they are not allocated
       in the externed block, and they are automatically shared. */
    if (sz == 0) {
      if (tag < 16) {
        Write(PREFIX_SMALL_BLOCK + tag);
      } else {
        writecode32(CODE_BLOCK32, hd);
      }
      goto next_item;
    }
    /* Check if already seen */
    if (Color_hd(hd) == Caml_blue) {
      uintnat d = obj_counter - (uintnat) Field(v, 0);
      if (d < 0x100) {
        writecode8(CODE_SHARED8, d);
      } else if (d < 0x10000) {
        writecode16(CODE_SHARED16, d);
      } else {
        writecode32(CODE_SHARED32, d);
      }
      goto next_item;
    }

    /* Output the contents of the object */
    switch(tag) {
    case String_tag: {
      mlsize_t len = caml_string_length(v);
      if (len < 0x20) {
        Write(PREFIX_SMALL_STRING + len);
      } else if (len < 0x100) {
        writecode8(CODE_STRING8, len);
      } else {
#ifdef ARCH_SIXTYFOUR
        if (len > 0xFFFFFB && (extern_flags & COMPAT_32))
          extern_failwith("output_value: string cannot be read back on "
                          "32-bit platform");
#endif
        writecode32(CODE_STRING32, len);
      }
      writeblock(String_val(v), len);
      size_32 += 1 + (len + 4) / 4;
      size_64 += 1 + (len + 8) / 8;
      extern_record_location(v);
      break;
    }
    case Double_tag: {
      if (sizeof(double) != 8)
        extern_invalid_argument("output_value: non-standard floats");
      Write(CODE_DOUBLE_NATIVE);
      writeblock_float8((double *) v, 1);
      size_32 += 1 + 2;
      size_64 += 1 + 1;
      extern_record_location(v);
      break;
    }
    case Double_array_tag: {
      mlsize_t nfloats;
      if (sizeof(double) != 8)
        extern_invalid_argument("output_value: non-standard floats");
      nfloats = Wosize_val(v) / Double_wosize;
      if (nfloats < 0x100) {
        writecode8(CODE_DOUBLE_ARRAY8_NATIVE, nfloats);
      } else {
#ifdef ARCH_SIXTYFOUR
        if (nfloats > 0x1FFFFF && (extern_flags & COMPAT_32))
          extern_failwith("output_value: float array cannot be read back on "
                          "32-bit platform");
#endif
        writecode32(CODE_DOUBLE_ARRAY32_NATIVE, nfloats);
      }
      writeblock_float8((double *) v, nfloats);
      size_32 += 1 + nfloats * 2;
      size_64 += 1 + nfloats;
      extern_record_location(v);
      break;
    }
    case Abstract_tag:
      extern_invalid_argument("output_value: abstract value (Abstract)");
      break;
    case Infix_tag:
      writecode32(CODE_INFIXPOINTER, Infix_offset_hd(hd));
      v = v - Infix_offset_hd(hd); /* PR#5772 */
      continue;
    case Custom_tag: {
      uintnat sz_32, sz_64;
      char * ident = Custom_ops_val(v)->identifier;
      void (*serialize)(value v, uintnat * wsize_32,
                        uintnat * wsize_64)
        = Custom_ops_val(v)->serialize;
      if (serialize == NULL)
        extern_invalid_argument("output_value: abstract value (Custom)");
      Write(CODE_CUSTOM);
      writeblock(ident, strlen(ident) + 1);
      Custom_ops_val(v)->serialize(v, &sz_32, &sz_64);
      size_32 += 2 + ((sz_32 + 3) >> 2);  /* header + ops + data */
      size_64 += 2 + ((sz_64 + 7) >> 3);
      extern_record_location(v);
      break;
    }
    default: {
      value field0;
      if (tag < 16 && sz < 8) {
        Write(PREFIX_SMALL_BLOCK + tag + (sz << 4));
#ifdef ARCH_SIXTYFOUR
      } else if (hd >= ((uintnat)1 << 32)) {
        /* Is this case useful?  The overflow check in extern_value will fail.*/
        writecode64(CODE_BLOCK64, Whitehd_hd (hd));
#endif
      } else {
#ifdef ARCH_SIXTYFOUR
        if (sz > 0x3FFFFF && (extern_flags & COMPAT_32))
          extern_failwith("output_value: array cannot be read back on "
                          "32-bit platform");
#endif
        writecode32(CODE_BLOCK32, Whitehd_hd (hd));
      }
      size_32 += 1 + sz;
      size_64 += 1 + sz;
      field0 = Field(v, 0);
      extern_record_location(v);
      /* Remember that we still have to serialize fields 1 ... sz - 1 */
      if (sz > 1) {
        sp++;
        if (sp >= extern_stack_limit) sp = extern_resize_stack(sp);
        sp->v = &Field(v,1);
        sp->count = sz-1;
      }
      /* Continue serialization with the first field */
      v = field0;
      continue;
    }
    }
  }
  else if ((cf = extern_find_code((char *) v)) != NULL) {
    if ((extern_flags & CLOSURES) == 0)
      extern_invalid_argument("output_value: functional value");
    writecode32(CODE_CODEPOINTER, (char *) v - cf->code_start);
    writeblock((char *) cf->digest, 16);
  } else {
    extern_invalid_argument("output_value: abstract value (outside heap)");
  }
  next_item:
    /* Pop one more item to marshal, if any */
    if (sp == extern_stack) {
        /* We are done.   Cleanup the stack and leave the function */
        extern_free_stack();
        return;
    }
    v = *((sp->v)++);
    if (--(sp->count) == 0) sp--;
  }
  /* Never reached as function leaves with return */
}

static int extern_flag_values[] = { NO_SHARING, CLOSURES, COMPAT_32 };

static intnat extern_value(value v, value flags)
{
  intnat res_len;
  /* Parse flag list */
  extern_flags = caml_convert_flag_list(flags, extern_flag_values);
  /* Initializations */
  init_extern_trail();
  obj_counter = 0;
  size_32 = 0;
  size_64 = 0;
  /* Write magic number */
  write32(Intext_magic_number);
  /* Set aside space for the sizes */
  extern_ptr += 4*4;
  /* Marshal the object */
  extern_rec(v);
  /* Record end of output */
  close_extern_output();
  /* Undo the modifications done on externed blocks */
  extern_replay_trail();
  /* Write the sizes */
  res_len = extern_output_length();
#ifdef ARCH_SIXTYFOUR
  if (res_len >= ((intnat)1 << 32) ||
      size_32 >= ((intnat)1 << 32) || size_64 >= ((intnat)1 << 32)) {
    /* The object is so big its size cannot be written in the header.
       Besides, some of the array lengths or string lengths or shared offsets
       it contains may have overflowed the 32 bits used to write them. */
    free_extern_output();
    caml_failwith("output_value: object too big");
  }
#endif
  if (extern_userprovided_output != NULL)
    extern_ptr = extern_userprovided_output + 4;
  else {
    extern_ptr = extern_output_first->data + 4;
    extern_limit = extern_output_first->data + SIZE_EXTERN_OUTPUT_BLOCK;
  }
  write32(res_len - 5*4);
  write32(obj_counter);
  write32(size_32);
  write32(size_64);
  return res_len;
}

void caml_output_val(struct channel *chan, value v, value flags)
{
  struct output_block * blk, * nextblk;

  if (! caml_channel_binary_mode(chan))
    caml_failwith("output_value: not a binary channel");
  init_extern_output();
  extern_value(v, flags);
  /* During [caml_really_putblock], concurrent [caml_output_val] operations
     can take place (via signal handlers or context switching in systhreads),
     and [extern_output_first] may change. So, save it in a local variable. */
  blk = extern_output_first;
  while (blk != NULL) {
    caml_really_putblock(chan, blk->data, blk->end - blk->data);
    nextblk = blk->next;
    free(blk);
    blk = nextblk;
  }
}

CAMLprim value caml_output_value(value vchan, value v, value flags)
{
  CAMLparam3 (vchan, v, flags);
  struct channel * channel = Channel(vchan);

  Lock(channel);
  caml_output_val(channel, v, flags);
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_output_value_to_string(value v, value flags)
{
  intnat len, ofs;
  value res;
  struct output_block * blk, * nextblk;

  init_extern_output();
  len = extern_value(v, flags);
  /* PR#4030: it is prudent to save extern_output_first before allocating
     the result, as in caml_output_val */
  blk = extern_output_first;
  res = caml_alloc_string(len);
  ofs = 0;
  while (blk != NULL) {
    int n = blk->end - blk->data;
    memmove(&Byte(res, ofs), blk->data, n);
    ofs += n;
    nextblk = blk->next;
    free(blk);
    blk = nextblk;
  }
  return res;
}

CAMLprim value caml_output_value_to_buffer(value buf, value ofs, value len,
                                           value v, value flags)
{
  intnat len_res;
  extern_userprovided_output = &Byte(buf, Long_val(ofs));
  extern_ptr = extern_userprovided_output;
  extern_limit = extern_userprovided_output + Long_val(len);
  len_res = extern_value(v, flags);
  return Val_long(len_res);
}

CAMLexport void caml_output_value_to_malloc(value v, value flags,
                                            /*out*/ char ** buf,
                                            /*out*/ intnat * len)
{
  intnat len_res;
  char * res;
  struct output_block * blk;

  init_extern_output();
  len_res = extern_value(v, flags);
  res = malloc(len_res);
  if (res == NULL) extern_out_of_memory();
  *buf = res;
  *len = len_res;
  for (blk = extern_output_first; blk != NULL; blk = blk->next) {
    int n = blk->end - blk->data;
    memmove(res, blk->data, n);
    res += n;
  }
  free_extern_output();
}

CAMLexport intnat caml_output_value_to_block(value v, value flags,
                                             char * buf, intnat len)
{
  intnat len_res;
  extern_userprovided_output = buf;
  extern_ptr = extern_userprovided_output;
  extern_limit = extern_userprovided_output + len;
  len_res = extern_value(v, flags);
  return len_res;
}

/* Functions for writing user-defined marshallers */

CAMLexport void caml_serialize_int_1(int i)
{
  if (extern_ptr + 1 > extern_limit) grow_extern_output(1);
  extern_ptr[0] = i;
  extern_ptr += 1;
}

CAMLexport void caml_serialize_int_2(int i)
{
  if (extern_ptr + 2 > extern_limit) grow_extern_output(2);
  extern_ptr[0] = i >> 8;
  extern_ptr[1] = i;
  extern_ptr += 2;
}

CAMLexport void caml_serialize_int_4(int32 i)
{
  if (extern_ptr + 4 > extern_limit) grow_extern_output(4);
  extern_ptr[0] = i >> 24;
  extern_ptr[1] = i >> 16;
  extern_ptr[2] = i >> 8;
  extern_ptr[3] = i;
  extern_ptr += 4;
}

CAMLexport void caml_serialize_int_8(int64 i)
{
  caml_serialize_block_8(&i, 1);
}

CAMLexport void caml_serialize_float_4(float f)
{
  caml_serialize_block_4(&f, 1);
}

CAMLexport void caml_serialize_float_8(double f)
{
  caml_serialize_block_float_8(&f, 1);
}

CAMLexport void caml_serialize_block_1(void * data, intnat len)
{
  if (extern_ptr + len > extern_limit) grow_extern_output(len);
  memmove(extern_ptr, data, len);
  extern_ptr += len;
}

CAMLexport void caml_serialize_block_2(void * data, intnat len)
{
  if (extern_ptr + 2 * len > extern_limit) grow_extern_output(2 * len);
#ifndef ARCH_BIG_ENDIAN
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 2, q += 2)
      Reverse_16(q, p);
    extern_ptr = q;
  }
#else
  memmove(extern_ptr, data, len * 2);
  extern_ptr += len * 2;
#endif
}

CAMLexport void caml_serialize_block_4(void * data, intnat len)
{
  if (extern_ptr + 4 * len > extern_limit) grow_extern_output(4 * len);
#ifndef ARCH_BIG_ENDIAN
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 4, q += 4)
      Reverse_32(q, p);
    extern_ptr = q;
  }
#else
  memmove(extern_ptr, data, len * 4);
  extern_ptr += len * 4;
#endif
}

CAMLexport void caml_serialize_block_8(void * data, intnat len)
{
  if (extern_ptr + 8 * len > extern_limit) grow_extern_output(8 * len);
#ifndef ARCH_BIG_ENDIAN
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 8, q += 8)
      Reverse_64(q, p);
    extern_ptr = q;
  }
#else
  memmove(extern_ptr, data, len * 8);
  extern_ptr += len * 8;
#endif
}

CAMLexport void caml_serialize_block_float_8(void * data, intnat len)
{
  if (extern_ptr + 8 * len > extern_limit) grow_extern_output(8 * len);
#if ARCH_FLOAT_ENDIANNESS == 0x01234567
  memmove(extern_ptr, data, len * 8);
  extern_ptr += len * 8;
#elif ARCH_FLOAT_ENDIANNESS == 0x76543210
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 8, q += 8)
      Reverse_64(q, p);
    extern_ptr = q;
  }
#else
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 8, q += 8)
      Permute_64(q, 0x01234567, p, ARCH_FLOAT_ENDIANNESS);
    extern_ptr = q;
  }
#endif
}

/* Find where a code pointer comes from */

static struct code_fragment * extern_find_code(char *addr)
{
  int i;
  for (i = caml_code_fragments_table.size - 1; i >= 0; i--) {
    struct code_fragment * cf = caml_code_fragments_table.contents[i];
    if (! cf->digest_computed) {
      caml_md5_block(cf->digest, cf->code_start, cf->code_end - cf->code_start);
      cf->digest_computed = 1;
    }
    if (cf->code_start <= addr && addr < cf->code_end) return cf;
  }
  return NULL;
}
