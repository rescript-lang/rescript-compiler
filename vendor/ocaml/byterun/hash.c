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

/* The generic hashing primitive */

/* The interface of this file is in "mlvalues.h" (for [caml_hash_variant])
   and in "hash.h" (for the other exported functions). */

#include "caml/mlvalues.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/hash.h"

/* The new implementation, based on MurmurHash 3,
     http://code.google.com/p/smhasher/  */

#define ROTL32(x,n) ((x) << n | (x) >> (32-n))

#define MIX(h,d) \
  d *= 0xcc9e2d51; \
  d = ROTL32(d, 15); \
  d *= 0x1b873593; \
  h ^= d; \
  h = ROTL32(h, 13); \
  h = h * 5 + 0xe6546b64;

#define FINAL_MIX(h) \
  h ^= h >> 16; \
  h *= 0x85ebca6b; \
  h ^= h >> 13; \
  h *= 0xc2b2ae35; \
  h ^= h >> 16;

CAMLexport uint32 caml_hash_mix_uint32(uint32 h, uint32 d)
{
  MIX(h, d);
  return h;
}

/* Mix a platform-native integer. */

CAMLexport uint32 caml_hash_mix_intnat(uint32 h, intnat d)
{
  uint32 n;
#ifdef ARCH_SIXTYFOUR
  /* Mix the low 32 bits and the high 32 bits, in a way that preserves
     32/64 compatibility: we want n = (uint32) d
     if d is in the range [-2^31, 2^31-1]. */
  n = (d >> 32) ^ (d >> 63) ^ d;
  /* If 0 <= d < 2^31:   d >> 32 = 0     d >> 63 = 0
     If -2^31 <= d < 0:  d >> 32 = -1    d >> 63 = -1
     In both cases, n = (uint32) d.  */
#else
  n = d;
#endif
  MIX(h, n);
  return h;
}

/* Mix a 64-bit integer. */

CAMLexport uint32 caml_hash_mix_int64(uint32 h, int64 d)
{
  uint32 hi = (uint32) (d >> 32), lo = (uint32) d;
  MIX(h, lo);
  MIX(h, hi);
  return h;
}

/* Mix a double-precision float.
   Treats +0.0 and -0.0 identically.
   Treats all NaNs identically.
*/

CAMLexport uint32 caml_hash_mix_double(uint32 hash, double d)
{
  union {
    double d;
#if defined(ARCH_BIG_ENDIAN) || (defined(__arm__) && !defined(__ARM_EABI__))
    struct { uint32 h; uint32 l; } i;
#else
    struct { uint32 l; uint32 h; } i;
#endif
  } u;
  uint32 h, l;
  /* Convert to two 32-bit halves */
  u.d = d;
  h = u.i.h; l = u.i.l;
  /* Normalize NaNs */
  if ((h & 0x7FF00000) == 0x7FF00000 && (l | (h & 0xFFFFF)) != 0) {
    h = 0x7FF00000;
    l = 0x00000001;
  }
  /* Normalize -0 into +0 */
  else if (h == 0x80000000 && l == 0) {
    h = 0;
  }
  MIX(hash, l);
  MIX(hash, h);
  return hash;
}

/* Mix a single-precision float.
   Treats +0.0 and -0.0 identically.
   Treats all NaNs identically.
*/

CAMLexport uint32 caml_hash_mix_float(uint32 hash, float d)
{
  union {
    float f;
    uint32 i;
  } u;
  uint32 n;
  /* Convert to int32 */
  u.f = d;  n = u.i;
  /* Normalize NaNs */
  if ((n & 0x7F800000) == 0x7F800000 && (n & 0x007FFFFF) != 0) {
    n = 0x7F800001;
  }
  /* Normalize -0 into +0 */
  else if (n == 0x80000000) {
    n = 0;
  }
  MIX(hash, n);
  return hash;
}

/* Mix an OCaml string */

CAMLexport uint32 caml_hash_mix_string(uint32 h, value s)
{
  mlsize_t len = caml_string_length(s);
  mlsize_t i;
  uint32 w;

  /* Mix by 32-bit blocks (little-endian) */
  for (i = 0; i + 4 <= len; i += 4) {
#ifdef ARCH_BIG_ENDIAN
    w = Byte_u(s, i)
        | (Byte_u(s, i+1) << 8)
        | (Byte_u(s, i+2) << 16)
        | (Byte_u(s, i+3) << 24);
#else
    w = *((uint32 *) &Byte_u(s, i));
#endif
    MIX(h, w);
  }
  /* Finish with up to 3 bytes */
  w = 0;
  switch (len & 3) {
  case 3: w  = Byte_u(s, i+2) << 16;   /* fallthrough */
  case 2: w |= Byte_u(s, i+1) << 8;    /* fallthrough */
  case 1: w |= Byte_u(s, i);
          MIX(h, w);
  default: /*skip*/;     /* len & 3 == 0, no extra bytes, do nothing */
  }
  /* Finally, mix in the length.  Ignore the upper 32 bits, generally 0. */
  h ^= (uint32) len;
  return h;
}

/* Maximal size of the queue used for breadth-first traversal.  */
#define HASH_QUEUE_SIZE 256
/* Maximal number of Forward_tag links followed in one step */
#define MAX_FORWARD_DEREFERENCE 1000

/* The generic hash function */

CAMLprim value caml_hash(value count, value limit, value seed, value obj)
{
  value queue[HASH_QUEUE_SIZE]; /* Queue of values to examine */
  intnat rd;                    /* Position of first value in queue */
  intnat wr;                    /* One past position of last value in queue */
  intnat sz;                    /* Max number of values to put in queue */
  intnat num;                   /* Max number of meaningful values to see */
  uint32 h;                     /* Rolling hash */
  value v;
  mlsize_t i, len;

  sz = Long_val(limit);
  if (sz < 0 || sz > HASH_QUEUE_SIZE) sz = HASH_QUEUE_SIZE;
  num = Long_val(count);
  h = Int_val(seed);
  queue[0] = obj; rd = 0; wr = 1;

  while (rd < wr && num > 0) {
    v = queue[rd++];
  again:
    if (Is_long(v)) {
      h = caml_hash_mix_intnat(h, v);
      num--;
    }
    else if (Is_in_value_area(v)) {
      switch (Tag_val(v)) {
      case String_tag:
        h = caml_hash_mix_string(h, v);
        num--;
        break;
      case Double_tag:
        h = caml_hash_mix_double(h, Double_val(v));
        num--;
        break;
      case Double_array_tag:
        for (i = 0, len = Wosize_val(v) / Double_wosize; i < len; i++) {
          h = caml_hash_mix_double(h, Double_field(v, i));
          num--;
          if (num <= 0) break;
        }
        break;
      case Abstract_tag:
        /* Block contents unknown.  Do nothing. */
        break;
      case Infix_tag:
        /* Mix in the offset to distinguish different functions from
           the same mutually-recursive definition */
        h = caml_hash_mix_uint32(h, Infix_offset_val(v));
        v = v - Infix_offset_val(v);
        goto again;
      case Forward_tag:
        /* PR#6361: we can have a loop here, so limit the number of
           Forward_tag links being followed */
        for (i = MAX_FORWARD_DEREFERENCE; i > 0; i--) {
          v = Forward_val(v);
          if (Is_long(v) || !Is_in_value_area(v) || Tag_val(v) != Forward_tag)
            goto again;
        }
        /* Give up on this object and move to the next */
        break;
      case Object_tag:
        h = caml_hash_mix_intnat(h, Oid_val(v));
        num--;
        break;
      case Custom_tag:
        /* If no hashing function provided, do nothing. */
        /* Only use low 32 bits of custom hash, for 32/64 compatibility */
        if (Custom_ops_val(v)->hash != NULL) {
          uint32 n = (uint32) Custom_ops_val(v)->hash(v);
          h = caml_hash_mix_uint32(h, n);
          num--;
        }
        break;
      default:
        /* Mix in the tag and size, but do not count this towards [num] */
        h = caml_hash_mix_uint32(h, Whitehd_hd(Hd_val(v)));
        /* Copy fields into queue, not exceeding the total size [sz] */
        for (i = 0, len = Wosize_val(v); i < len; i++) {
          if (wr >= sz) break;
          queue[wr++] = Field(v, i);
        }
        break;
      }
    } else {
      /* v is a pointer outside the heap, probably a code pointer.
         Shall we count it?  Let's say yes by compatibility with old code. */
      h = caml_hash_mix_intnat(h, v);
      num--;
    }
  }
  /* Final mixing of bits */
  FINAL_MIX(h);
  /* Fold result to the range [0, 2^30-1] so that it is a nonnegative
     OCaml integer both on 32 and 64-bit platforms. */
  return Val_int(h & 0x3FFFFFFFU);
}

/* The old implementation */

static uintnat hash_accu;
static intnat hash_univ_limit, hash_univ_count;

static void hash_aux(value obj);

CAMLprim value caml_hash_univ_param(value count, value limit, value obj)
{
  hash_univ_limit = Long_val(limit);
  hash_univ_count = Long_val(count);
  hash_accu = 0;
  hash_aux(obj);
  return Val_long(hash_accu & 0x3FFFFFFF);
  /* The & has two purposes: ensure that the return value is positive
     and give the same result on 32 bit and 64 bit architectures. */
}

#define Alpha 65599
#define Beta 19
#define Combine(new)  (hash_accu = hash_accu * Alpha + (new))
#define Combine_small(new) (hash_accu = hash_accu * Beta + (new))

static void hash_aux(value obj)
{
  unsigned char * p;
  mlsize_t i, j;
  tag_t tag;

  hash_univ_limit--;
  if (hash_univ_count < 0 || hash_univ_limit < 0) return;

 again:
  if (Is_long(obj)) {
    hash_univ_count--;
    Combine(Long_val(obj));
    return;
  }

  /* Pointers into the heap are well-structured blocks. So are atoms.
     We can inspect the block contents. */

  Assert (Is_block (obj));
  if (Is_in_value_area(obj)) {
    tag = Tag_val(obj);
    switch (tag) {
    case String_tag:
      hash_univ_count--;
      i = caml_string_length(obj);
      for (p = &Byte_u(obj, 0); i > 0; i--, p++)
        Combine_small(*p);
      break;
    case Double_tag:
      /* For doubles, we inspect their binary representation, LSB first.
         The results are consistent among all platforms with IEEE floats. */
      hash_univ_count--;
#ifdef ARCH_BIG_ENDIAN
      for (p = &Byte_u(obj, sizeof(double) - 1), i = sizeof(double);
           i > 0;
           p--, i--)
#else
      for (p = &Byte_u(obj, 0), i = sizeof(double);
           i > 0;
           p++, i--)
#endif
        Combine_small(*p);
      break;
    case Double_array_tag:
      hash_univ_count--;
      for (j = 0; j < Bosize_val(obj); j += sizeof(double)) {
#ifdef ARCH_BIG_ENDIAN
      for (p = &Byte_u(obj, j + sizeof(double) - 1), i = sizeof(double);
           i > 0;
           p--, i--)
#else
      for (p = &Byte_u(obj, j), i = sizeof(double);
           i > 0;
           p++, i--)
#endif
        Combine_small(*p);
      }
      break;
    case Abstract_tag:
      /* We don't know anything about the contents of the block.
         Better do nothing. */
      break;
    case Infix_tag:
      hash_aux(obj - Infix_offset_val(obj));
      break;
    case Forward_tag:
      obj = Forward_val (obj);
      goto again;
    case Object_tag:
      hash_univ_count--;
      Combine(Oid_val(obj));
      break;
    case Custom_tag:
      /* If no hashing function provided, do nothing */
      if (Custom_ops_val(obj)->hash != NULL) {
        hash_univ_count--;
        Combine(Custom_ops_val(obj)->hash(obj));
      }
      break;
    default:
      hash_univ_count--;
      Combine_small(tag);
      i = Wosize_val(obj);
      while (i != 0) {
        i--;
        hash_aux(Field(obj, i));
      }
      break;
    }
    return;
  }

  /* Otherwise, obj is a pointer outside the heap, to an object with
     a priori unknown structure. Use its physical address as hash key. */
  Combine((intnat) obj);
}

/* Hashing variant tags */

CAMLexport value caml_hash_variant(char const * tag)
{
  value accu;
  /* Same hashing algorithm as in ../typing/btype.ml, function hash_variant */
  for (accu = Val_int(0); *tag != 0; tag++)
    accu = Val_int(223 * Int_val(accu) + *((unsigned char *) tag));
#ifdef ARCH_SIXTYFOUR
  accu = accu & Val_long(0x7FFFFFFFL);
#endif
  /* Force sign extension of bit 31 for compatibility between 32 and 64-bit
     platforms */
  return (int32) accu;
}
