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

/* Buffered input/output */

#ifndef CAML_IO_H
#define CAML_IO_H

#include "misc.h"
#include "mlvalues.h"

#ifndef IO_BUFFER_SIZE
#define IO_BUFFER_SIZE 65536
#endif

#if defined(_WIN32)
typedef __int64 file_offset;
#elif defined(HAS_OFF_T)
#include <sys/types.h>
typedef off_t file_offset;
#else
typedef long file_offset;
#endif

struct channel {
  int fd;                       /* Unix file descriptor */
  file_offset offset;           /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  void * mutex;                 /* Placeholder for mutex (for systhreads) */
  struct channel * next, * prev;/* Double chaining of channels (flush_all) */
  int revealed;                 /* For Cash only */
  int old_revealed;             /* For Cash only */
  int refcount;                 /* For flush_all and for Cash */
  int flags;                    /* Bitfield */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
};

enum {
  CHANNEL_FLAG_FROM_SOCKET = 1  /* For Windows */
};

/* For an output channel:
     [offset] is the absolute position of the beginning of the buffer [buff].
   For an input channel:
     [offset] is the absolute position of the logical end of the buffer, [max].
*/

/* Functions and macros that can be called from C.  Take arguments of
   type struct channel *.  No locking is performed. */

#define putch(channel, ch) do{                                            \
  if ((channel)->curr >= (channel)->end) caml_flush_partial(channel);     \
  *((channel)->curr)++ = (ch);                                            \
}while(0)

#define getch(channel)                                                      \
  ((channel)->curr >= (channel)->max                                        \
   ? caml_refill(channel)                                                   \
   : (unsigned char) *((channel)->curr)++)

CAMLextern struct channel * caml_open_descriptor_in (int);
CAMLextern struct channel * caml_open_descriptor_out (int);
CAMLextern void caml_close_channel (struct channel *);
CAMLextern int caml_channel_binary_mode (struct channel *);
CAMLextern value caml_alloc_channel(struct channel *chan);

CAMLextern int caml_flush_partial (struct channel *);
CAMLextern void caml_flush (struct channel *);
CAMLextern void caml_putword (struct channel *, uint32);
CAMLextern int caml_putblock (struct channel *, char *, intnat);
CAMLextern void caml_really_putblock (struct channel *, char *, intnat);

CAMLextern unsigned char caml_refill (struct channel *);
CAMLextern uint32 caml_getword (struct channel *);
CAMLextern int caml_getblock (struct channel *, char *, intnat);
CAMLextern int caml_really_getblock (struct channel *, char *, intnat);

/* Extract a struct channel * from the heap object representing it */

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

/* The locking machinery */

CAMLextern void (*caml_channel_mutex_free) (struct channel *);
CAMLextern void (*caml_channel_mutex_lock) (struct channel *);
CAMLextern void (*caml_channel_mutex_unlock) (struct channel *);
CAMLextern void (*caml_channel_mutex_unlock_exn) (void);

CAMLextern struct channel * caml_all_opened_channels;

#define Lock(channel) \
  if (caml_channel_mutex_lock != NULL) (*caml_channel_mutex_lock)(channel)
#define Unlock(channel) \
  if (caml_channel_mutex_unlock != NULL) (*caml_channel_mutex_unlock)(channel)
#define Unlock_exn() \
  if (caml_channel_mutex_unlock_exn != NULL) (*caml_channel_mutex_unlock_exn)()

/* Conversion between file_offset and int64 */

#define Val_file_offset(fofs) caml_copy_int64(fofs)
#define File_offset_val(v) ((file_offset) Int64_val(v))

#endif /* CAML_IO_H */
