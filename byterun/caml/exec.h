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

/* exec.h : format of executable bytecode files */

#ifndef CAML_EXEC_H
#define CAML_EXEC_H

/* Executable bytecode files are composed of a number of sections,
   identified by 4-character names.  A table of contents at the
   end of the file lists the section names along with their sizes,
   in the order in which they appear in the file:

   offset 0 --->  initial junk
                  data for section 1
                  data for section 2
                  ...
                  data for section N
                  table of contents:
                    descriptor for section 1
                    ...
                    descriptor for section N
                  trailer
 end of file --->
*/

/* Structure of t.o.c. entries
   Numerical quantities are 32-bit unsigned integers, big endian */

struct section_descriptor {
  char name[4];                 /* Section name */
  uint32 len;                   /* Length of data in bytes */
};

/* Structure of the trailer. */

struct exec_trailer {
  uint32 num_sections;          /* Number of sections */
  char magic[12];               /* The magic number */
  struct section_descriptor * section; /* Not part of file */
};

#define TRAILER_SIZE (4+12)

/* Magic number for this release */

#define EXEC_MAGIC "Caml1999X011"


#endif /* CAML_EXEC_H */
