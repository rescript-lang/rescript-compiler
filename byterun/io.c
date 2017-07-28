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

/* Buffered input/output. */

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <string.h>
#include <sys/types.h>
#include "caml/config.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#ifdef __CYGWIN__
#include </usr/include/io.h>
#endif
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"
#include "caml/sys.h"

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

/* Hooks for locking channels */

CAMLexport void (*caml_channel_mutex_free) (struct channel *) = NULL;
CAMLexport void (*caml_channel_mutex_lock) (struct channel *) = NULL;
CAMLexport void (*caml_channel_mutex_unlock) (struct channel *) = NULL;
CAMLexport void (*caml_channel_mutex_unlock_exn) (void) = NULL;

/* List of opened channels */
CAMLexport struct channel * caml_all_opened_channels = NULL;

/* Basic functions over type struct channel *.
   These functions can be called directly from C.
   No locking is performed. */

/* Functions shared between input and output */

CAMLexport struct channel * caml_open_descriptor_in(int fd)
{
  struct channel * channel;

  channel = (struct channel *) caml_stat_alloc(sizeof(struct channel));
  channel->fd = fd;
  caml_enter_blocking_section();
  channel->offset = lseek(fd, 0, SEEK_CUR);
  caml_leave_blocking_section();
  channel->curr = channel->max = channel->buff;
  channel->end = channel->buff + IO_BUFFER_SIZE;
  channel->mutex = NULL;
  channel->revealed = 0;
  channel->old_revealed = 0;
  channel->refcount = 0;
  channel->flags = 0;
  channel->next = caml_all_opened_channels;
  channel->prev = NULL;
  if (caml_all_opened_channels != NULL)
    caml_all_opened_channels->prev = channel;
  caml_all_opened_channels = channel;
  return channel;
}

CAMLexport struct channel * caml_open_descriptor_out(int fd)
{
  struct channel * channel;

  channel = caml_open_descriptor_in(fd);
  channel->max = NULL;
  return channel;
}

static void unlink_channel(struct channel *channel)
{
  if (channel->prev == NULL) {
    Assert (channel == caml_all_opened_channels);
    caml_all_opened_channels = caml_all_opened_channels->next;
    if (caml_all_opened_channels != NULL)
      caml_all_opened_channels->prev = NULL;
  } else {
    channel->prev->next = channel->next;
    if (channel->next != NULL) channel->next->prev = channel->prev;
  }
}

CAMLexport void caml_close_channel(struct channel *channel)
{
  close(channel->fd);
  if (channel->refcount > 0) return;
  if (caml_channel_mutex_free != NULL) (*caml_channel_mutex_free)(channel);
  unlink_channel(channel);
  caml_stat_free(channel);
}

CAMLexport file_offset caml_channel_size(struct channel *channel)
{
  file_offset offset;
  file_offset end;
  int fd;

  /* We extract data from [channel] before dropping the OCaml lock, in case
     someone else touches the block. */
  fd = channel->fd;
  offset = channel->offset;
  caml_enter_blocking_section();
  end = lseek(fd, 0, SEEK_END);
  if (end == -1 || lseek(fd, offset, SEEK_SET) != offset) {
    caml_leave_blocking_section();
    caml_sys_error(NO_ARG);
  }
  caml_leave_blocking_section();
  return end;
}

CAMLexport int caml_channel_binary_mode(struct channel *channel)
{
#if defined(_WIN32) || defined(__CYGWIN__)
  int oldmode = setmode(channel->fd, O_BINARY);
  if (oldmode == O_TEXT) setmode(channel->fd, O_TEXT);
  return oldmode == O_BINARY;
#else
  return 1;
#endif
}

/* Output */

#ifndef EINTR
#define EINTR (-1)
#endif
#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif

static int do_write(int fd, char *p, int n)
{
  int retcode;

again:
  caml_enter_blocking_section();
  retcode = write(fd, p, n);
  caml_leave_blocking_section();
  if (retcode == -1) {
    if (errno == EINTR) goto again;
    if ((errno == EAGAIN || errno == EWOULDBLOCK) && n > 1) {
      /* We couldn't do a partial write here, probably because
         n <= PIPE_BUF and POSIX says that writes of less than
         PIPE_BUF characters must be atomic.
         We first try again with a partial write of 1 character.
         If that fails too, we'll raise Sys_blocked_io below. */
      n = 1; goto again;
    }
  }
  if (retcode == -1) caml_sys_io_error(NO_ARG);
  return retcode;
}

/* Attempt to flush the buffer. This will make room in the buffer for
   at least one character. Returns true if the buffer is empty at the
   end of the flush, or false if some data remains in the buffer.
 */

CAMLexport int caml_flush_partial(struct channel *channel)
{
  int towrite, written;

  towrite = channel->curr - channel->buff;
  if (towrite > 0) {
    written = do_write(channel->fd, channel->buff, towrite);
    channel->offset += written;
    if (written < towrite)
      memmove(channel->buff, channel->buff + written, towrite - written);
    channel->curr -= written;
  }
  return (channel->curr == channel->buff);
}

/* Flush completely the buffer. */

CAMLexport void caml_flush(struct channel *channel)
{
  while (! caml_flush_partial(channel)) /*nothing*/;
}

/* Output data */

CAMLexport void caml_putword(struct channel *channel, uint32 w)
{
  if (! caml_channel_binary_mode(channel))
    caml_failwith("output_binary_int: not a binary channel");
  putch(channel, w >> 24);
  putch(channel, w >> 16);
  putch(channel, w >> 8);
  putch(channel, w);
}

CAMLexport int caml_putblock(struct channel *channel, char *p, intnat len)
{
  int n, free, towrite, written;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  free = channel->end - channel->curr;
  if (n < free) {
    /* Write request small enough to fit in buffer: transfer to buffer. */
    memmove(channel->curr, p, n);
    channel->curr += n;
    return n;
  } else {
    /* Write request overflows buffer (or just fills it up): transfer whatever
       fits to buffer and write the buffer */
    memmove(channel->curr, p, free);
    towrite = channel->end - channel->buff;
    written = do_write(channel->fd, channel->buff, towrite);
    if (written < towrite)
      memmove(channel->buff, channel->buff + written, towrite - written);
    channel->offset += written;
    channel->curr = channel->end - written;
    return free;
  }
}

CAMLexport void caml_really_putblock(struct channel *channel,
                                     char *p, intnat len)
{
  int written;
  while (len > 0) {
    written = caml_putblock(channel, p, len);
    p += written;
    len -= written;
  }
}

CAMLexport void caml_seek_out(struct channel *channel, file_offset dest)
{
  caml_flush(channel);
  caml_enter_blocking_section();
  if (lseek(channel->fd, dest, SEEK_SET) != dest) {
    caml_leave_blocking_section();
    caml_sys_error(NO_ARG);
  }
  caml_leave_blocking_section();
  channel->offset = dest;
}

CAMLexport file_offset caml_pos_out(struct channel *channel)
{
  return channel->offset + (file_offset)(channel->curr - channel->buff);
}

/* Input */

/* caml_do_read is exported for Cash */
CAMLexport int caml_do_read(int fd, char *p, unsigned int n)
{
  int retcode;

  do {
    caml_enter_blocking_section();
    retcode = read(fd, p, n);
#if defined(_WIN32)
    if (retcode == -1 && errno == ENOMEM && n > 16384){
      retcode = read(fd, p, 16384);
    }
#endif
    caml_leave_blocking_section();
  } while (retcode == -1 && errno == EINTR);
  if (retcode == -1) caml_sys_io_error(NO_ARG);
  return retcode;
}

CAMLexport unsigned char caml_refill(struct channel *channel)
{
  int n;

  n = caml_do_read(channel->fd, channel->buff, channel->end - channel->buff);
  if (n == 0) caml_raise_end_of_file();
  channel->offset += n;
  channel->max = channel->buff + n;
  channel->curr = channel->buff + 1;
  return (unsigned char)(channel->buff[0]);
}

CAMLexport uint32 caml_getword(struct channel *channel)
{
  int i;
  uint32 res;

  if (! caml_channel_binary_mode(channel))
    caml_failwith("input_binary_int: not a binary channel");
  res = 0;
  for(i = 0; i < 4; i++) {
    res = (res << 8) + getch(channel);
  }
  return res;
}

CAMLexport int caml_getblock(struct channel *channel, char *p, intnat len)
{
  int n, avail, nread;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  avail = channel->max - channel->curr;
  if (n <= avail) {
    memmove(p, channel->curr, n);
    channel->curr += n;
    return n;
  } else if (avail > 0) {
    memmove(p, channel->curr, avail);
    channel->curr += avail;
    return avail;
  } else {
    nread = caml_do_read(channel->fd, channel->buff,
                         channel->end - channel->buff);
    channel->offset += nread;
    channel->max = channel->buff + nread;
    if (n > nread) n = nread;
    memmove(p, channel->buff, n);
    channel->curr = channel->buff + n;
    return n;
  }
}

CAMLexport int caml_really_getblock(struct channel *chan, char *p, intnat n)
{
  int r;
  while (n > 0) {
    r = caml_getblock(chan, p, n);
    if (r == 0) break;
    p += r;
    n -= r;
  }
  return (n == 0);
}

CAMLexport void caml_seek_in(struct channel *channel, file_offset dest)
{
  if (dest >= channel->offset - (channel->max - channel->buff) &&
      dest <= channel->offset) {
    channel->curr = channel->max - (channel->offset - dest);
  } else {
    caml_enter_blocking_section();
    if (lseek(channel->fd, dest, SEEK_SET) != dest) {
      caml_leave_blocking_section();
      caml_sys_error(NO_ARG);
    }
    caml_leave_blocking_section();
    channel->offset = dest;
    channel->curr = channel->max = channel->buff;
  }
}

CAMLexport file_offset caml_pos_in(struct channel *channel)
{
  return channel->offset - (file_offset)(channel->max - channel->curr);
}

CAMLexport intnat caml_input_scan_line(struct channel *channel)
{
  char * p;
  int n;

  p = channel->curr;
  do {
    if (p >= channel->max) {
      /* No more characters available in the buffer */
      if (channel->curr > channel->buff) {
        /* Try to make some room in the buffer by shifting the unread
           portion at the beginning */
        memmove(channel->buff, channel->curr, channel->max - channel->curr);
        n = channel->curr - channel->buff;
        channel->curr -= n;
        channel->max -= n;
        p -= n;
      }
      if (channel->max >= channel->end) {
        /* Buffer is full, no room to read more characters from the input.
           Return the number of characters in the buffer, with negative
           sign to indicate that no newline was encountered. */
        return -(channel->max - channel->curr);
      }
      /* Fill the buffer as much as possible */
      n = caml_do_read(channel->fd, channel->max, channel->end - channel->max);
      if (n == 0) {
        /* End-of-file encountered. Return the number of characters in the
           buffer, with negative sign since we haven't encountered
           a newline. */
        return -(channel->max - channel->curr);
      }
      channel->offset += n;
      channel->max += n;
    }
  } while (*p++ != '\n');
  /* Found a newline. Return the length of the line, newline included. */
  return (p - channel->curr);
}

/* OCaml entry points for the I/O functions.  Wrap struct channel *
   objects into a heap-allocated object.  Perform locking
   and unlocking around the I/O operations. */
/* FIXME CAMLexport, but not in io.h  exported for Cash ? */
CAMLexport void caml_finalize_channel(value vchan)
{
  struct channel * chan = Channel(vchan);
  if (--chan->refcount > 0) return;
  if (caml_channel_mutex_free != NULL) (*caml_channel_mutex_free)(chan);
  unlink_channel(chan);
  caml_stat_free(chan);
}

static int compare_channel(value vchan1, value vchan2)
{
  struct channel * chan1 = Channel(vchan1);
  struct channel * chan2 = Channel(vchan2);
  return (chan1 == chan2) ? 0 : (chan1 < chan2) ? -1 : 1;
}

static intnat hash_channel(value vchan)
{
  return (intnat) (Channel(vchan));
}

static struct custom_operations channel_operations = {
  "_chan",
  caml_finalize_channel,
  compare_channel,
  hash_channel,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

CAMLexport value caml_alloc_channel(struct channel *chan)
{
  value res;
  chan->refcount++;             /* prevent finalization during next alloc */
  res = caml_alloc_custom(&channel_operations, sizeof(struct channel *),
                          1, 1000);
  Channel(res) = chan;
  return res;
}

CAMLprim value caml_ml_open_descriptor_in(value fd)
{
  return caml_alloc_channel(caml_open_descriptor_in(Int_val(fd)));
}

CAMLprim value caml_ml_open_descriptor_out(value fd)
{
  return caml_alloc_channel(caml_open_descriptor_out(Int_val(fd)));
}

#define Pair_tag 0

CAMLprim value caml_ml_out_channels_list (value unit)
{
  CAMLparam0 ();
  CAMLlocal3 (res, tail, chan);
  struct channel * channel;

  res = Val_emptylist;
  for (channel = caml_all_opened_channels;
       channel != NULL;
       channel = channel->next)
    /* Testing channel->fd >= 0 looks unnecessary, as
       caml_ml_close_channel changes max when setting fd to -1. */
    if (channel->max == NULL) {
      chan = caml_alloc_channel (channel);
      tail = res;
      res = caml_alloc_small (2, Pair_tag);
      Field (res, 0) = chan;
      Field (res, 1) = tail;
    }
  CAMLreturn (res);
}

CAMLprim value caml_channel_descriptor(value vchannel)
{
  int fd = Channel(vchannel)->fd;
  if (fd == -1) { errno = EBADF; caml_sys_error(NO_ARG); }
  return Val_int(fd);
}

CAMLprim value caml_ml_close_channel(value vchannel)
{
  int result;
  int do_syscall;
  int fd;

  /* For output channels, must have flushed before */
  struct channel * channel = Channel(vchannel);
  if (channel->fd != -1){
    fd = channel->fd;
    channel->fd = -1;
    do_syscall = 1;
  }else{
    do_syscall = 0;
    result = 0;
  }
  /* Ensure that every read or write on the channel will cause an
     immediate caml_flush_partial or caml_refill, thus raising a Sys_error
     exception */
  channel->curr = channel->max = channel->end;

  if (do_syscall) {
    caml_enter_blocking_section();
    result = close(fd);
    caml_leave_blocking_section();
  }

  if (result == -1) caml_sys_error (NO_ARG);
  return Val_unit;
}

/* EOVERFLOW is the Unix98 error indicating that a file position or file
   size is not representable.
   ERANGE is the ANSI C error indicating that some argument to some
   function is out of range.  This is less precise than EOVERFLOW,
   but guaranteed to be defined on all ANSI C environments. */
#ifndef EOVERFLOW
#define EOVERFLOW ERANGE
#endif

CAMLprim value caml_ml_channel_size(value vchannel)
{
  file_offset size = caml_channel_size(Channel(vchannel));
  if (size > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  return Val_long(size);
}

CAMLprim value caml_ml_channel_size_64(value vchannel)
{
  return Val_file_offset(caml_channel_size(Channel(vchannel)));
}

CAMLprim value caml_ml_set_binary_mode(value vchannel, value mode)
{
#if defined(_WIN32) || defined(__CYGWIN__)
  struct channel * channel = Channel(vchannel);
  if (setmode(channel->fd, Bool_val(mode) ? O_BINARY : O_TEXT) == -1)
    caml_sys_error(NO_ARG);
#endif
  return Val_unit;
}

/*
   If the channel is closed, DO NOT raise a "bad file descriptor"
   exception, but do nothing (the buffer is already empty).
   This is because some libraries will flush at exit, even on
   file descriptors that may be closed.
*/

CAMLprim value caml_ml_flush_partial(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  int res;

  if (channel->fd == -1) CAMLreturn(Val_true);
  Lock(channel);
  res = caml_flush_partial(channel);
  Unlock(channel);
  CAMLreturn (Val_bool(res));
}

CAMLprim value caml_ml_flush(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);

  if (channel->fd == -1) CAMLreturn(Val_unit);
  Lock(channel);
  caml_flush(channel);
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_char(value vchannel, value ch)
{
  CAMLparam2 (vchannel, ch);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  putch(channel, Long_val(ch));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_int(value vchannel, value w)
{
  CAMLparam2 (vchannel, w);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  caml_putword(channel, Long_val(w));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_partial(value vchannel, value buff, value start,
                                      value length)
{
  CAMLparam4 (vchannel, buff, start, length);
  struct channel * channel = Channel(vchannel);
  int res;

  Lock(channel);
  res = caml_putblock(channel, &Byte(buff, Long_val(start)), Long_val(length));
  Unlock(channel);
  CAMLreturn (Val_int(res));
}

CAMLprim value caml_ml_output(value vchannel, value buff, value start,
                              value length)
{
  CAMLparam4 (vchannel, buff, start, length);
  struct channel * channel = Channel(vchannel);
  intnat pos = Long_val(start);
  intnat len = Long_val(length);

  Lock(channel);
    while (len > 0) {
      int written = caml_putblock(channel, &Byte(buff, pos), len);
      pos += written;
      len -= written;
    }
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_out(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  caml_seek_out(channel, Long_val(pos));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_out_64(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  caml_seek_out(channel, File_offset_val(pos));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_pos_out(value vchannel)
{
  file_offset pos = caml_pos_out(Channel(vchannel));
  if (pos > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  return Val_long(pos);
}

CAMLprim value caml_ml_pos_out_64(value vchannel)
{
  return Val_file_offset(caml_pos_out(Channel(vchannel)));
}

CAMLprim value caml_ml_input_char(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  unsigned char c;

  Lock(channel);
  c = getch(channel);
  Unlock(channel);
  CAMLreturn (Val_long(c));
}

CAMLprim value caml_ml_input_int(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  intnat i;

  Lock(channel);
  i = caml_getword(channel);
  Unlock(channel);
#ifdef ARCH_SIXTYFOUR
  i = (i << 32) >> 32;          /* Force sign extension */
#endif
  CAMLreturn (Val_long(i));
}

CAMLprim value caml_ml_input(value vchannel, value buff, value vstart,
                             value vlength)
{
  CAMLparam4 (vchannel, buff, vstart, vlength);
  struct channel * channel = Channel(vchannel);
  intnat start, len;
  int n, avail, nread;

  Lock(channel);
  /* We cannot call caml_getblock here because buff may move during
     caml_do_read */
  start = Long_val(vstart);
  len = Long_val(vlength);
  n = len >= INT_MAX ? INT_MAX : (int) len;
  avail = channel->max - channel->curr;
  if (n <= avail) {
    memmove(&Byte(buff, start), channel->curr, n);
    channel->curr += n;
  } else if (avail > 0) {
    memmove(&Byte(buff, start), channel->curr, avail);
    channel->curr += avail;
    n = avail;
  } else {
    nread = caml_do_read(channel->fd, channel->buff,
                         channel->end - channel->buff);
    channel->offset += nread;
    channel->max = channel->buff + nread;
    if (n > nread) n = nread;
    memmove(&Byte(buff, start), channel->buff, n);
    channel->curr = channel->buff + n;
  }
  Unlock(channel);
  CAMLreturn (Val_long(n));
}

CAMLprim value caml_ml_seek_in(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  caml_seek_in(channel, Long_val(pos));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_in_64(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  Lock(channel);
  caml_seek_in(channel, File_offset_val(pos));
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_pos_in(value vchannel)
{
  file_offset pos = caml_pos_in(Channel(vchannel));
  if (pos > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  return Val_long(pos);
}

CAMLprim value caml_ml_pos_in_64(value vchannel)
{
  return Val_file_offset(caml_pos_in(Channel(vchannel)));
}

CAMLprim value caml_ml_input_scan_line(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  intnat res;

  Lock(channel);
  res = caml_input_scan_line(channel);
  Unlock(channel);
  CAMLreturn (Val_long(res));
}
