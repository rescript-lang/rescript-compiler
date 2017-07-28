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
#include <caml/fail.h>
#include "unixsupport.h"

#ifdef HAS_TERMIOS

#include <termios.h>
#include <errno.h>

static struct termios terminal_status;

enum { Bool, Enum, Speed, Char, End };

enum { Input, Output };

#define iflags ((long)(&terminal_status.c_iflag))
#define oflags ((long)(&terminal_status.c_oflag))
#define cflags ((long)(&terminal_status.c_cflag))
#define lflags ((long)(&terminal_status.c_lflag))

/* Number of fields in the terminal_io record field. Cf. unix.mli */

#define NFIELDS 38

/* Structure of the terminal_io record. Cf. unix.mli */

static long terminal_io_descr[] = {
  /* Input modes */
  Bool, iflags, IGNBRK,
  Bool, iflags, BRKINT,
  Bool, iflags, IGNPAR,
  Bool, iflags, PARMRK,
  Bool, iflags, INPCK,
  Bool, iflags, ISTRIP,
  Bool, iflags, INLCR,
  Bool, iflags, IGNCR,
  Bool, iflags, ICRNL,
  Bool, iflags, IXON,
  Bool, iflags, IXOFF,
  /* Output modes */
  Bool, oflags, OPOST,
  /* Control modes */
  Speed, Output,
  Speed, Input,
  Enum, cflags, 5, 4, CSIZE, CS5, CS6, CS7, CS8,
  Enum, cflags, 1, 2, CSTOPB, 0, CSTOPB,
  Bool, cflags, CREAD,
  Bool, cflags, PARENB,
  Bool, cflags, PARODD,
  Bool, cflags, HUPCL,
  Bool, cflags, CLOCAL,
  /* Local modes */
  Bool, lflags, ISIG,
  Bool, lflags, ICANON,
  Bool, lflags, NOFLSH,
  Bool, lflags, ECHO,
  Bool, lflags, ECHOE,
  Bool, lflags, ECHOK,
  Bool, lflags, ECHONL,
  /* Control characters */
  Char, VINTR,
  Char, VQUIT,
  Char, VERASE,
  Char, VKILL,
  Char, VEOF,
  Char, VEOL,
  Char, VMIN,
  Char, VTIME,
  Char, VSTART,
  Char, VSTOP,
  End
};

#undef iflags
#undef oflags
#undef cflags
#undef lflags

static struct {
  speed_t speed;
  int baud;
} speedtable[] = {

  /* standard speeds */
  {B0,       0},
  {B50,      50},
  {B75,      75},
  {B110,     110},
  {B134,     134},
  {B150,     150},
#ifdef B200
  /* Shouldn't need to be ifdef'd but I'm not sure it's available everywhere. */
  {B200,     200},
#endif
  {B300,     300},
  {B600,     600},
  {B1200,    1200},
  {B1800,    1800},
  {B2400,    2400},
  {B4800,    4800},
  {B9600,    9600},
  {B19200,   19200},
  {B38400,   38400},

  /* usual extensions */
#ifdef B57600
  {B57600,   57600},
#endif
#ifdef B115200
  {B115200,  115200},
#endif
#ifdef B230400
  {B230400,  230400},
#endif

  /* Linux extensions */
#ifdef B460800
  {B460800,  460800},
#endif
#ifdef B500000
  {B500000,  500000},
#endif
#ifdef B576000
  {B576000,  576000},
#endif
#ifdef B921600
  {B921600,  921600},
#endif
#ifdef B1000000
  {B1000000, 1000000},
#endif
#ifdef B1152000
  {B1152000, 1152000},
#endif
#ifdef B1500000
  {B1500000, 1500000},
#endif
#ifdef B2000000
  {B2000000, 2000000},
#endif
#ifdef B2500000
  {B2500000, 2500000},
#endif
#ifdef B3000000
  {B3000000, 3000000},
#endif
#ifdef B3500000
  {B3500000, 3500000},
#endif
#ifdef B4000000
  {B4000000, 4000000},
#endif

  /* MacOS extensions */
#ifdef B7200
  {B7200,    7200},
#endif
#ifdef B14400
  {B14400,   14400},
#endif
#ifdef B28800
  {B28800,   28800},
#endif
#ifdef B76800
  {B76800,   76800},
#endif

  /* Cygwin extensions (in addition to the Linux ones) */
#ifdef B128000
  {B128000,  128000},
#endif
#ifdef B256000
  {B256000,  256000},
#endif
};

#define NSPEEDS (sizeof(speedtable) / sizeof(speedtable[0]))

static void encode_terminal_status(value *dst)
{
  long * pc;
  int i;

  for(pc = terminal_io_descr; *pc != End; dst++) {
    switch(*pc++) {
    case Bool:
      { int * src = (int *) (*pc++);
        int msk = *pc++;
        *dst = Val_bool(*src & msk);
        break; }
    case Enum:
      { int * src = (int *) (*pc++);
        int ofs = *pc++;
        int num = *pc++;
        int msk = *pc++;
        for (i = 0; i < num; i++) {
          if ((*src & msk) == pc[i]) {
            *dst = Val_int(i + ofs);
            break;
          }
        }
        pc += num;
        break; }
    case Speed:
      { int which = *pc++;
        speed_t speed = 0;
        *dst = Val_int(9600);   /* in case no speed in speedtable matches */
        switch (which) {
        case Output:
          speed = cfgetospeed(&terminal_status); break;
        case Input:
          speed = cfgetispeed(&terminal_status); break;
        }
        for (i = 0; i < NSPEEDS; i++) {
          if (speed == speedtable[i].speed) {
            *dst = Val_int(speedtable[i].baud);
            break;
          }
        }
        break; }
    case Char:
      { int which = *pc++;
        *dst = Val_int(terminal_status.c_cc[which]);
        break; }
    }
  }
}

static void decode_terminal_status(value *src)
{
  long * pc;
  int i;

  for (pc = terminal_io_descr; *pc != End; src++) {
    switch(*pc++) {
    case Bool:
      { int * dst = (int *) (*pc++);
        int msk = *pc++;
        if (Bool_val(*src))
          *dst |= msk;
        else
          *dst &= ~msk;
        break; }
    case Enum:
      { int * dst = (int *) (*pc++);
        int ofs = *pc++;
        int num = *pc++;
        int msk = *pc++;
        i = Int_val(*src) - ofs;
        if (i >= 0 && i < num) {
          *dst = (*dst & ~msk) | pc[i];
        } else {
          unix_error(EINVAL, "tcsetattr", Nothing);
        }
        pc += num;
        break; }
    case Speed:
      { int which = *pc++;
        int baud = Int_val(*src);
        int res = 0;
        for (i = 0; i < NSPEEDS; i++) {
          if (baud == speedtable[i].baud) {
            switch (which) {
            case Output:
              res = cfsetospeed(&terminal_status, speedtable[i].speed); break;
            case Input:
              res = cfsetispeed(&terminal_status, speedtable[i].speed); break;
            }
            if (res == -1) uerror("tcsetattr", Nothing);
            goto ok;
          }
        }
        unix_error(EINVAL, "tcsetattr", Nothing);
      ok:
        break; }
    case Char:
      { int which = *pc++;
        terminal_status.c_cc[which] = Int_val(*src);
        break; }
    }
  }
}

CAMLprim value unix_tcgetattr(value fd)
{
  value res;

  if (tcgetattr(Int_val(fd), &terminal_status) == -1)
    uerror("tcgetattr", Nothing);
  res = alloc_tuple(NFIELDS);
  encode_terminal_status(&Field(res, 0));
  return res;
}

static int when_flag_table[] = {
  TCSANOW, TCSADRAIN, TCSAFLUSH
};

CAMLprim value unix_tcsetattr(value fd, value when, value arg)
{
  if (tcgetattr(Int_val(fd), &terminal_status) == -1)
    uerror("tcsetattr", Nothing);
  decode_terminal_status(&Field(arg, 0));
  if (tcsetattr(Int_val(fd),
                when_flag_table[Int_val(when)],
                &terminal_status) == -1)
    uerror("tcsetattr", Nothing);
  return Val_unit;
}

CAMLprim value unix_tcsendbreak(value fd, value delay)
{
  if (tcsendbreak(Int_val(fd), Int_val(delay)) == -1)
    uerror("tcsendbreak", Nothing);
  return Val_unit;
}

#if defined(__ANDROID__)
CAMLprim value unix_tcdrain(value fd)
{ invalid_argument("tcdrain not implemented"); }
#else
CAMLprim value unix_tcdrain(value fd)
{
  if (tcdrain(Int_val(fd)) == -1) uerror("tcdrain", Nothing);
  return Val_unit;
}
#endif

static int queue_flag_table[] = {
  TCIFLUSH, TCOFLUSH, TCIOFLUSH
};

CAMLprim value unix_tcflush(value fd, value queue)
{
  if (tcflush(Int_val(fd), queue_flag_table[Int_val(queue)]) == -1)
    uerror("tcflush", Nothing);
  return Val_unit;
}

static int action_flag_table[] = {
  TCOOFF, TCOON, TCIOFF, TCION
};

CAMLprim value unix_tcflow(value fd, value action)
{
  if (tcflow(Int_val(fd), action_flag_table[Int_val(action)]) == -1)
    uerror("tcflow", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_tcgetattr(value fd)
{ invalid_argument("tcgetattr not implemented"); }

CAMLprim value unix_tcsetattr(value fd, value when, value arg)
{ invalid_argument("tcsetattr not implemented"); }

CAMLprim value unix_tcsendbreak(value fd, value delay)
{ invalid_argument("tcsendbreak not implemented"); }

CAMLprim value unix_tcdrain(value fd)
{ invalid_argument("tcdrain not implemented"); }

CAMLprim value unix_tcflush(value fd, value queue)
{ invalid_argument("tcflush not implemented"); }

CAMLprim value unix_tcflow(value fd, value action)
{ invalid_argument("tcflow not implemented"); }

#endif
