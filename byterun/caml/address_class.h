/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* Classification of addresses for GC and runtime purposes. */

#ifndef CAML_ADDRESS_CLASS_H
#define CAML_ADDRESS_CLASS_H

#include "misc.h"
#include "mlvalues.h"

/* Use the following macros to test an address for the different classes
   it might belong to. */

#define Is_young(val) \
  (Assert (Is_block (val)), \
   (addr)(val) < (addr)caml_young_end && (addr)(val) > (addr)caml_young_start)

#define Is_in_heap(a) (Classify_addr(a) & In_heap)

#define Is_in_heap_or_young(a) (Classify_addr(a) & (In_heap | In_young))

#define Is_in_value_area(a)                                     \
  (Classify_addr(a) & (In_heap | In_young | In_static_data))

#define Is_in_code_area(pc) \
 (    ((char *)(pc) >= caml_code_area_start && \
       (char *)(pc) <= caml_code_area_end)     \
   || (Classify_addr(pc) & In_code_area) )

#define Is_in_static_data(a) (Classify_addr(a) & In_static_data)

/***********************************************************************/
/* The rest of this file is private and may change without notice. */

extern char *caml_young_start, *caml_young_end;
extern char * caml_code_area_start, * caml_code_area_end;

#define Not_in_heap 0
#define In_heap 1
#define In_young 2
#define In_static_data 4
#define In_code_area 8

#ifdef ARCH_SIXTYFOUR

/* 64 bits: Represent page table as a sparse hash table */
int caml_page_table_lookup(void * addr);
#define Classify_addr(a) (caml_page_table_lookup((void *)(a)))

#else

/* 32 bits: Represent page table as a 2-level array */
#define Pagetable2_log 11
#define Pagetable2_size (1 << Pagetable2_log)
#define Pagetable1_log (Page_log + Pagetable2_log)
#define Pagetable1_size (1 << (32 - Pagetable1_log))
CAMLextern unsigned char * caml_page_table[Pagetable1_size];

#define Pagetable_index1(a) (((uintnat)(a)) >> Pagetable1_log)
#define Pagetable_index2(a) \
  ((((uintnat)(a)) >> Page_log) & (Pagetable2_size - 1))
#define Classify_addr(a) \
  caml_page_table[Pagetable_index1(a)][Pagetable_index2(a)]

#endif

int caml_page_table_add(int kind, void * start, void * end);
int caml_page_table_remove(int kind, void * start, void * end);
int caml_page_table_initialize(mlsize_t bytesize);

#endif /* CAML_ADDRESS_CLASS_H */
