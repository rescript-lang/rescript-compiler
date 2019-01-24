/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Damien Doligez, projet Para, INRIA Rocquencourt            */
/*                                                                        */
/*   Copyright 1997 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Operations on weak arrays */

#ifndef CAML_WEAK_H
#define CAML_WEAK_H

#ifdef CAML_INTERNALS

#include "mlvalues.h"

extern value caml_ephe_list_head;
extern value caml_ephe_none;


/** The first field 0:  weak list;
       second field 1:  data;
       others       2..:  keys;

    A weak pointer is an ephemeron with the data at caml_ephe_none
    If fields are added, don't forget to update weak.ml [additional_values].
 */

#define CAML_EPHE_LINK_OFFSET 0
#define CAML_EPHE_DATA_OFFSET 1
#define CAML_EPHE_FIRST_KEY 2


/* In the header, in order to let major_gc.c
   and weak.c see the body of the function */
static inline void caml_ephe_clean (value v){
  value child;
  int release_data = 0;
  mlsize_t size, i;
  header_t hd;
  CAMLassert(caml_gc_phase == Phase_clean);

  hd = Hd_val (v);
  size = Wosize_hd (hd);
  for (i = 2; i < size; i++){
    child = Field (v, i);
  ephemeron_again:
    if (child != caml_ephe_none
        && Is_block (child) && Is_in_heap_or_young (child)){
      if (Tag_val (child) == Forward_tag){
        value f = Forward_val (child);
        if (Is_block (f)) {
          if (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
              || Tag_val (f) == Lazy_tag || Tag_val (f) == Double_tag){
            /* Do not short-circuit the pointer. */
          }else{
            Field (v, i) = child = f;
            if (Is_block (f) && Is_young (f))
              add_to_ephe_ref_table(&caml_ephe_ref_table, v, i);
            goto ephemeron_again;
          }
        }
      }
      if (Is_white_val (child) && !Is_young (child)){
        release_data = 1;
        Field (v, i) = caml_ephe_none;
      }
    }
  }

  child = Field (v, 1);
  if(child != caml_ephe_none){
      if (release_data){
        Field (v, 1) = caml_ephe_none;
      } else {
        /* The mark phase must have marked it */
        CAMLassert( !(Is_block (child) && Is_in_heap (child)
                  && Is_white_val (child)) );
      }
  }
}

#endif /* CAML_INTERNALS */

#endif /* CAML_WEAK_H */
