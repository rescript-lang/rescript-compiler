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

/* Interface with C primitives. */

#ifndef CAML_PRIMS_H
#define CAML_PRIMS_H

typedef value (*c_primitive)();

extern c_primitive caml_builtin_cprim[];
extern char * caml_names_of_builtin_cprim[];

extern struct ext_table caml_prim_table;
#ifdef DEBUG
extern struct ext_table caml_prim_name_table;
#endif

#define Primitive(n) ((c_primitive)(caml_prim_table.contents[n]))

extern char * caml_section_table;
extern asize_t caml_section_table_size;

#endif /* CAML_PRIMS_H */
