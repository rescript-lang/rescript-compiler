/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*                 Jeremie Dimino, Jane Street Group, LLC              */
/*                                                                     */
/*  Copyright 2015 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* This file is used by the configure test program nanosecond_stat.c
   and stat.c in this directory */

#if HAS_NANOSECOND_STAT == 1
#  define NSEC(buf, field) buf->st_##field##tim.tv_nsec
#elif HAS_NANOSECOND_STAT == 2
#  define NSEC(buf, field) buf->st_##field##timespec.tv_nsec
#elif HAS_NANOSECOND_STAT == 3
#  define NSEC(buf, field) buf->st_##field##timensec
#else
#  define NSEC(buf, field) 0
#endif
