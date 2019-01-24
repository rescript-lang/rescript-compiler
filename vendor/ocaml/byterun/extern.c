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

/* Structured output */

/* The interface of this file is "caml/intext.h" */

#include <string.h>
#include "caml/alloc.h"
#include "caml/config.h"
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

CAMLnoreturn_start
static void extern_out_of_memory(void)
CAMLnoreturn_end;

CAMLnoreturn_start
static void extern_invalid_argument(char *msg)
CAMLnoreturn_end;

CAMLnoreturn_start
static void extern_failwith(char *msg)
CAMLnoreturn_end;

CAMLnoreturn_start
static void extern_stack_overflow(void)
CAMLnoreturn_end;

static void extern_replay_trail(void);
static void free_extern_output(void);

/* Free the extern stack if needed */
static void extern_free_stack(void)
{
  if (extern_stack != extern_stack_init) {
    caml_stat_free(extern_stack);
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
    newstack = caml_stat_alloc_noexc(sizeof(struct extern_item) * newsize);
    if (newstack == NULL) extern_stack_overflow();
    memcpy(newstack, extern_stack_init,
           sizeof(struct extern_item) * EXTERN_STACK_INIT_SIZE);
  } else {
    newstack = caml_stat_resize_noexc(extern_stack,
                                      sizeof(struct extern_item) * newsize);
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
    caml_stat_free(blk);
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
    struct trail_block * new_block = caml_stat_alloc_noexc(sizeof(struct trail_block));
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
  extern_output_first = caml_stat_alloc_noexc(sizeof(struct output_block));
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
    caml_stat_free(blk);
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
  blk = caml_stat_alloc_noexc(sizeof(struct output_block) + extra);
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
  caml_gc_message (0x04, "Stack overflow in marshaling value\n");
  extern_replay_trail();
  free_extern_output();
  caml_raise_out_of_memory();
}

/* Conversion to big-endian */

static inline void store16(char * dst, int n)
{
  dst[0] = n >> 8;  dst[1] = n;
}

static inline void store32(char * dst, intnat n)
{
  dst[0] = n >> 24;  dst[1] = n >> 16;  dst[2] = n >> 8;  dst[3] = n;
}

static inline void store64(char * dst, int64_t n)
{
  dst[0] = n >> 56;  dst[1] = n >> 48;  dst[2] = n >> 40;  dst[3] = n >> 32;
  dst[4] = n >> 24;  dst[5] = n >> 16;  dst[6] = n >> 8;   dst[7] = n;
}

/* Write characters, integers, and blocks in the output buffer */

static inline void write(int c)
{
  if (extern_ptr >= extern_limit) grow_extern_output(1);
  *extern_ptr++ = c;
}

static void writeblock(const char * data, intnat len)
{
  if (extern_ptr + len > extern_limit) grow_extern_output(len);
  memcpy(extern_ptr, data, len);
  extern_ptr += len;
}

static inline void writeblock_float8(const double * data, intnat ndoubles)
{
#if ARCH_FLOAT_ENDIANNESS == 0x01234567 || ARCH_FLOAT_ENDIANNESS == 0x76543210
  writeblock((const char *) data, ndoubles * 8);
#else
  caml_serialize_block_float_8(data, ndoubles);
#endif
}

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
  store16(extern_ptr + 1, val);
  extern_ptr += 3;
}

static void writecode32(int code, intnat val)
{
  if (extern_ptr + 5 > extern_limit) grow_extern_output(5);
  extern_ptr[0] = code;
  store32(extern_ptr + 1, val);
  extern_ptr += 5;
}

#ifdef ARCH_SIXTYFOUR
static void writecode64(int code, intnat val)
{
  if (extern_ptr + 9 > extern_limit) grow_extern_output(9);
  extern_ptr[0] = code;
  store64(extern_ptr + 1, val);
  extern_ptr += 9;
}
#endif

/* Marshal the given value in the output buffer */

int caml_extern_allow_out_of_heap = 0;

static void extern_rec(value v)
{
  struct code_fragment * cf;
  struct extern_item * sp;
  sp = extern_stack;

  while(1) {
  if (Is_long(v)) {
    intnat n = Long_val(v);
    if (n >= 0 && n < 0x40) {
      write(PREFIX_SMALL_INT + n);
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
  if (Is_in_value_area(v) || caml_extern_allow_out_of_heap) {
    header_t hd = Hd_val(v);
    tag_t tag = Tag_hd(hd);
    mlsize_t sz = Wosize_hd(hd);

    if (tag == Forward_tag) {
      value f = Forward_val (v);
      if (Is_block (f)
          && (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
              || Tag_val (f) == Lazy_tag
#ifdef FLAT_FLOAT_ARRAY
              || Tag_val (f) == Double_tag
#endif
              )){
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
        write(PREFIX_SMALL_BLOCK + tag);
      } else {
#ifdef WITH_PROFINFO
        writecode32(CODE_BLOCK32, Hd_no_profinfo(hd));
#else
        writecode32(CODE_BLOCK32, hd);
#endif
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
#ifdef ARCH_SIXTYFOUR
      } else if (d >= (uintnat)1 << 32) {
        writecode64(CODE_SHARED64, d);
#endif
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
        write(PREFIX_SMALL_STRING + len);
      } else if (len < 0x100) {
        writecode8(CODE_STRING8, len);
      } else {
#ifdef ARCH_SIXTYFOUR
        if (len > 0xFFFFFB && (extern_flags & COMPAT_32))
          extern_failwith("output_value: string cannot be read back on "
                          "32-bit platform");
        if (len < (uintnat)1 << 32)
          writecode32(CODE_STRING32, len);
        else
          writecode64(CODE_STRING64, len);
#else
        writecode32(CODE_STRING32, len);
#endif
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
      write(CODE_DOUBLE_NATIVE);
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
        if (nfloats < (uintnat) 1 << 32)
          writecode32(CODE_DOUBLE_ARRAY32_NATIVE, nfloats);
        else
          writecode64(CODE_DOUBLE_ARRAY64_NATIVE, nfloats);
#else
        writecode32(CODE_DOUBLE_ARRAY32_NATIVE, nfloats);
#endif
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
      void (*serialize)(value v, uintnat * bsize_32,
                        uintnat * bsize_64)
        = Custom_ops_val(v)->serialize;
      if (serialize == NULL)
        extern_invalid_argument("output_value: abstract value (Custom)");
      write(CODE_CUSTOM);
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
        write(PREFIX_SMALL_BLOCK + tag + (sz << 4));
      } else {
#ifdef ARCH_SIXTYFOUR
#ifdef WITH_PROFINFO
        header_t hd_erased = Hd_no_profinfo(hd);
#else
        header_t hd_erased = hd;
#endif
        if (sz > 0x3FFFFF && (extern_flags & COMPAT_32))
          extern_failwith("output_value: array cannot be read back on "
                          "32-bit platform");
        if (hd_erased < (uintnat)1 << 32)
          writecode32(CODE_BLOCK32, Whitehd_hd (hd_erased));
        else
          writecode64(CODE_BLOCK64, Whitehd_hd (hd_erased));
#else
        writecode32(CODE_BLOCK32, Whitehd_hd (hd));
#endif
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
  else if ((cf = caml_extern_find_code((char *) v)) != NULL) {
    if ((extern_flags & CLOSURES) == 0)
      extern_invalid_argument("output_value: functional value");
    writecode32(CODE_CODEPOINTER, (char *) v - cf->code_start);
    writeblock((const char *)cf->digest, 16);
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

static intnat extern_value(value v, value flags,
                           /*out*/ char header[32],
                           /*out*/ int * header_len)
{
  intnat res_len;
  /* Parse flag list */
  extern_flags = caml_convert_flag_list(flags, extern_flag_values);
  /* Initializations */
  init_extern_trail();
  obj_counter = 0;
  size_32 = 0;
  size_64 = 0;
  /* Marshal the object */
  extern_rec(v);
  /* Record end of output */
  close_extern_output();
  /* Undo the modifications done on externed blocks */
  extern_replay_trail();
  /* Write the header */
  res_len = extern_output_length();
#ifdef ARCH_SIXTYFOUR
  if (res_len >= ((intnat)1 << 32) ||
      size_32 >= ((intnat)1 << 32) || size_64 >= ((intnat)1 << 32)) {
    /* The object is too big for the small header format.
       Fail if we are in compat32 mode, or use big header. */
    if (extern_flags & COMPAT_32) {
      free_extern_output();
      caml_failwith("output_value: object too big to be read back on "
                    "32-bit platform");
    }
    store32(header, Intext_magic_number_big);
    store32(header + 4, 0);
    store64(header + 8, res_len);
    store64(header + 16, obj_counter);
    store64(header + 24, size_64);
    *header_len = 32;
    return res_len;
  }
#endif
  /* Use the small header format */
  store32(header, Intext_magic_number_small);
  store32(header + 4, res_len);
  store32(header + 8, obj_counter);
  store32(header + 12, size_32);
  store32(header + 16, size_64);
  *header_len = 20;
  return res_len;
}

void caml_output_val(struct channel *chan, value v, value flags)
{
  char header[32];
  int header_len;
  struct output_block * blk, * nextblk;

  if (! caml_channel_binary_mode(chan))
    caml_failwith("output_value: not a binary channel");
  init_extern_output();
  extern_value(v, flags, header, &header_len);
  /* During [caml_really_putblock], concurrent [caml_output_val] operations
     can take place (via signal handlers or context switching in systhreads),
     and [extern_output_first] may change. So, save it in a local variable. */
  blk = extern_output_first;
  caml_really_putblock(chan, header, header_len);
  while (blk != NULL) {
    caml_really_putblock(chan, blk->data, blk->end - blk->data);
    nextblk = blk->next;
    caml_stat_free(blk);
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
  char header[32];
  int header_len;
  intnat data_len, ofs;
  value res;
  struct output_block * blk, * nextblk;

  init_extern_output();
  data_len = extern_value(v, flags, header, &header_len);
  /* PR#4030: it is prudent to save extern_output_first before allocating
     the result, as in caml_output_val */
  blk = extern_output_first;
  res = caml_alloc_string(header_len + data_len);
  ofs = 0;
  memcpy(&Byte(res, ofs), header, header_len);
  ofs += header_len;
  while (blk != NULL) {
    int n = blk->end - blk->data;
    memcpy(&Byte(res, ofs), blk->data, n);
    ofs += n;
    nextblk = blk->next;
    caml_stat_free(blk);
    blk = nextblk;
  }
  return res;
}

CAMLexport intnat caml_output_value_to_block(value v, value flags,
                                             char * buf, intnat len)
{
  char header[32];
  int header_len;
  intnat data_len;
  /* At this point we don't know the size of the header.
     Guess that it is small, and fix up later if not. */
  extern_userprovided_output = buf + 20;
  extern_ptr = extern_userprovided_output;
  extern_limit = buf + len;
  data_len = extern_value(v, flags, header, &header_len);
  if (header_len != 20) {
    /* Bad guess!  Need to shift the output to make room for big header.
       Make sure there is room. */
    if (header_len + data_len > len)
      caml_failwith("Marshal.to_buffer: buffer overflow");
    memmove(buf + header_len, buf + 20, data_len);
  }
  memcpy(buf, header, header_len);
  return header_len + data_len;
}

CAMLprim value caml_output_value_to_buffer(value buf, value ofs, value len,
                                           value v, value flags)
{
  intnat l =
    caml_output_value_to_block(v, flags,
                               &Byte(buf, Long_val(ofs)), Long_val(len));
  return Val_long(l);
}

CAMLexport void caml_output_value_to_malloc(value v, value flags,
                                            /*out*/ char ** buf,
                                            /*out*/ intnat * len)
{
  char header[32];
  int header_len;
  intnat data_len;
  char * res;
  struct output_block * blk;

  init_extern_output();
  data_len = extern_value(v, flags, header, &header_len);
  res = caml_stat_alloc_noexc(header_len + data_len);
  if (res == NULL) extern_out_of_memory();
  *buf = res;
  *len = header_len + data_len;
  memcpy(res, header, header_len);
  res += header_len;
  for (blk = extern_output_first; blk != NULL; blk = blk->next) {
    int n = blk->end - blk->data;
    memcpy(res, blk->data, n);
    res += n;
  }
  free_extern_output();
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
  store16(extern_ptr, i);
  extern_ptr += 2;
}

CAMLexport void caml_serialize_int_4(int32_t i)
{
  if (extern_ptr + 4 > extern_limit) grow_extern_output(4);
  store32(extern_ptr, i);
  extern_ptr += 4;
}

CAMLexport void caml_serialize_int_8(int64_t i)
{
  if (extern_ptr + 8 > extern_limit) grow_extern_output(8);
  store64(extern_ptr, i);
  extern_ptr += 8;
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
  memcpy(extern_ptr, data, len);
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
  memcpy(extern_ptr, data, len * 2);
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
  memcpy(extern_ptr, data, len * 4);
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
  memcpy(extern_ptr, data, len * 8);
  extern_ptr += len * 8;
#endif
}

CAMLexport void caml_serialize_block_float_8(void * data, intnat len)
{
  if (extern_ptr + 8 * len > extern_limit) grow_extern_output(8 * len);
#if ARCH_FLOAT_ENDIANNESS == 0x01234567
  memcpy(extern_ptr, data, len * 8);
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

CAMLexport struct code_fragment * caml_extern_find_code(char *addr)
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
