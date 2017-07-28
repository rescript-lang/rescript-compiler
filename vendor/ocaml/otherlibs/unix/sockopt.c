/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with   */
/*  the special exception on linking described in file ../../LICENSE. */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "unixsupport.h"

#ifdef HAS_SOCKETS

#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/tcp.h>

#include "socketaddr.h"

#ifndef SO_DEBUG
#define SO_DEBUG (-1)
#endif
#ifndef SO_BROADCAST
#define SO_BROADCAST (-1)
#endif
#ifndef SO_REUSEADDR
#define SO_REUSEADDR (-1)
#endif
#ifndef SO_KEEPALIVE
#define SO_KEEPALIVE (-1)
#endif
#ifndef SO_DONTROUTE
#define SO_DONTROUTE (-1)
#endif
#ifndef SO_OOBINLINE
#define SO_OOBINLINE (-1)
#endif
#ifndef SO_ACCEPTCONN
#define SO_ACCEPTCONN (-1)
#endif
#ifndef SO_SNDBUF
#define SO_SNDBUF (-1)
#endif
#ifndef SO_RCVBUF
#define SO_RCVBUF (-1)
#endif
#ifndef SO_ERROR
#define SO_ERROR (-1)
#endif
#ifndef SO_TYPE
#define SO_TYPE (-1)
#endif
#ifndef SO_RCVLOWAT
#define SO_RCVLOWAT (-1)
#endif
#ifndef SO_SNDLOWAT
#define SO_SNDLOWAT (-1)
#endif
#ifndef SO_LINGER
#define SO_LINGER (-1)
#endif
#ifndef SO_RCVTIMEO
#define SO_RCVTIMEO (-1)
#endif
#ifndef SO_SNDTIMEO
#define SO_SNDTIMEO (-1)
#endif
#ifndef TCP_NODELAY
#define TCP_NODELAY (-1)
#endif
#ifndef SO_ERROR
#define SO_ERROR (-1)
#endif
#ifndef IPPROTO_IPV6
#define IPPROTO_IPV6 (-1)
#endif
#ifndef IPV6_V6ONLY
#define IPV6_V6ONLY (-1)
#endif

enum option_type {
  TYPE_BOOL = 0,
  TYPE_INT = 1,
  TYPE_LINGER = 2,
  TYPE_TIMEVAL = 3,
  TYPE_UNIX_ERROR = 4
};

struct socket_option {
  int level;
  int option;
};

/* Table of options, indexed by type */

static struct socket_option sockopt_bool[] = {
  { SOL_SOCKET, SO_DEBUG },
  { SOL_SOCKET, SO_BROADCAST },
  { SOL_SOCKET, SO_REUSEADDR },
  { SOL_SOCKET, SO_KEEPALIVE },
  { SOL_SOCKET, SO_DONTROUTE },
  { SOL_SOCKET, SO_OOBINLINE },
  { SOL_SOCKET, SO_ACCEPTCONN },
  { IPPROTO_TCP, TCP_NODELAY },
  { IPPROTO_IPV6, IPV6_V6ONLY}
};

static struct socket_option sockopt_int[] = {
  { SOL_SOCKET, SO_SNDBUF },
  { SOL_SOCKET, SO_RCVBUF },
  { SOL_SOCKET, SO_ERROR },
  { SOL_SOCKET, SO_TYPE },
  { SOL_SOCKET, SO_RCVLOWAT },
  { SOL_SOCKET, SO_SNDLOWAT } };

static struct socket_option sockopt_linger[] = {
  { SOL_SOCKET, SO_LINGER }
};

static struct socket_option sockopt_timeval[] = {
  { SOL_SOCKET, SO_RCVTIMEO },
  { SOL_SOCKET, SO_SNDTIMEO }
};

static struct socket_option sockopt_unix_error[] = {
  { SOL_SOCKET, SO_ERROR }
};

static struct socket_option * sockopt_table[] = {
  sockopt_bool,
  sockopt_int,
  sockopt_linger,
  sockopt_timeval,
  sockopt_unix_error
};

static char * getsockopt_fun_name[] = {
  "getsockopt",
  "getsockopt_int",
  "getsockopt_optint",
  "getsockopt_float",
  "getsockopt_error"
};

static char * setsockopt_fun_name[] = {
  "setsockopt",
  "setsockopt_int",
  "setsockopt_optint",
  "setsockopt_float",
  "setsockopt_error"
};

union option_value {
  int i;
  struct linger lg;
  struct timeval tv;
};

CAMLexport value
unix_getsockopt_aux(char * name,
                    enum option_type ty, int level, int option,
                    value socket)
{
  union option_value optval;
  socklen_param_type optsize;


  switch (ty) {
  case TYPE_BOOL:
  case TYPE_INT:
  case TYPE_UNIX_ERROR:
    optsize = sizeof(optval.i); break;
  case TYPE_LINGER:
    optsize = sizeof(optval.lg); break;
  case TYPE_TIMEVAL:
    optsize = sizeof(optval.tv); break;
  default:
    unix_error(EINVAL, name, Nothing);
  }

  if (getsockopt(Int_val(socket), level, option,
                 (void *) &optval, &optsize) == -1)
    uerror(name, Nothing);

  switch (ty) {
  case TYPE_BOOL:
    return Val_bool(optval.i);
  case TYPE_INT:
    return Val_int(optval.i);
  case TYPE_LINGER:
    if (optval.lg.l_onoff == 0) {
      return Val_int(0);        /* None */
    } else {
      value res = alloc_small(1, 0); /* Some */
      Field(res, 0) = Val_int(optval.lg.l_linger);
      return res;
    }
  case TYPE_TIMEVAL:
    return copy_double((double) optval.tv.tv_sec
                       + (double) optval.tv.tv_usec / 1e6);
  case TYPE_UNIX_ERROR:
    if (optval.i == 0) {
      return Val_int(0);        /* None */
    } else {
      value err, res;
      err = unix_error_of_code(optval.i);
      Begin_root(err);
        res = alloc_small(1, 0); /* Some */
        Field(res, 0) = err;
      End_roots();
      return res;
    }
  default:
    unix_error(EINVAL, name, Nothing);
  }
}

CAMLexport value
unix_setsockopt_aux(char * name,
                    enum option_type ty, int level, int option,
                    value socket, value val)
{
  union option_value optval;
  socklen_param_type optsize;
  double f;

  switch (ty) {
  case TYPE_BOOL:
  case TYPE_INT:
    optsize = sizeof(optval.i);
    optval.i = Int_val(val);
    break;
  case TYPE_LINGER:
    optsize = sizeof(optval.lg);
    optval.lg.l_onoff = Is_block (val);
    if (optval.lg.l_onoff)
      optval.lg.l_linger = Int_val (Field (val, 0));
    break;
  case TYPE_TIMEVAL:
    f = Double_val(val);
    optsize = sizeof(optval.tv);
    optval.tv.tv_sec = (int) f;
    optval.tv.tv_usec = (int) (1e6 * (f - optval.tv.tv_sec));
    break;
  case TYPE_UNIX_ERROR:
  default:
    unix_error(EINVAL, name, Nothing);
  }

  if (setsockopt(Int_val(socket), level, option,
                 (void *) &optval, optsize) == -1)
    uerror(name, Nothing);

  return Val_unit;
}

CAMLprim value unix_getsockopt(value vty, value vsocket, value voption)
{
  enum option_type ty = Int_val(vty);
  struct socket_option * opt = &(sockopt_table[ty][Int_val(voption)]);
  return unix_getsockopt_aux(getsockopt_fun_name[ty],
                             ty,
                             opt->level,
                             opt->option,
                             vsocket);
}

CAMLprim value unix_setsockopt(value vty, value vsocket, value voption,
                               value val)
{
  enum option_type ty = Int_val(vty);
  struct socket_option * opt = &(sockopt_table[ty][Int_val(voption)]);
  return unix_setsockopt_aux(setsockopt_fun_name[ty],
                             ty,
                             opt->level,
                             opt->option,
                             vsocket,
                             val);
}

#else

CAMLprim value unix_getsockopt(value vty, value socket, value option)
{ invalid_argument("getsockopt not implemented"); }

CAMLprim value unix_setsockopt(value vty, value socket, value option, value val)
{ invalid_argument("setsockopt not implemented"); }

#endif
