/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/io.h>
#include <caml/memory.h>
#include "unixsupport.h"
#include <fcntl.h>

extern intptr_t _get_osfhandle(int);
extern int _open_osfhandle(intptr_t, int);

int win_CRT_fd_of_filedescr(value handle)
{
  if (CRT_fd_val(handle) != NO_CRT_FD) {
    return CRT_fd_val(handle);
  } else {
    int fd = _open_osfhandle((intptr_t) Handle_val(handle), O_BINARY);
    if (fd == -1) uerror("channel_of_descr", Nothing);
    CRT_fd_val(handle) = fd;
    return fd;
  }
}

CAMLprim value win_inchannel_of_filedescr(value handle)
{
  CAMLparam1(handle);
  CAMLlocal1(vchan);
  struct channel * chan;

  chan = caml_open_descriptor_in(win_CRT_fd_of_filedescr(handle));
  if (Descr_kind_val(handle) == KIND_SOCKET)
    chan->flags |= CHANNEL_FLAG_FROM_SOCKET;
  vchan = caml_alloc_channel(chan);
  CAMLreturn(vchan);
}

CAMLprim value win_outchannel_of_filedescr(value handle)
{
  CAMLparam1(handle);
  CAMLlocal1(vchan);
  int fd;
  struct channel * chan;

  chan = caml_open_descriptor_out(win_CRT_fd_of_filedescr(handle));
  if (Descr_kind_val(handle) == KIND_SOCKET)
    chan->flags |= CHANNEL_FLAG_FROM_SOCKET;
  vchan = caml_alloc_channel(chan);
  CAMLreturn(vchan);
}

CAMLprim value win_filedescr_of_channel(value vchan)
{
  CAMLparam1(vchan);
  CAMLlocal1(fd);
  struct channel * chan;
  HANDLE h;

  chan = Channel(vchan);
  if (chan->fd == -1) uerror("descr_of_channel", Nothing);
  h = (HANDLE) _get_osfhandle(chan->fd);
  if (chan->flags & CHANNEL_FLAG_FROM_SOCKET)
    fd = win_alloc_socket((SOCKET) h);
  else
    fd = win_alloc_handle(h);
  CRT_fd_val(fd) = chan->fd;
  CAMLreturn(fd);
}

CAMLprim value win_handle_fd(value vfd)
{
  int crt_fd = Int_val(vfd);
  /* PR#4750: do not use the _or_socket variant as it can cause performance
     degradation and this function is only used with the standard
     handles 0, 1, 2, which are not sockets. */
  value res = win_alloc_handle((HANDLE) _get_osfhandle(crt_fd));
  CRT_fd_val(res) = crt_fd;
  return res;
}
