/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/*** This module replaced camlinternalMod completely. 
    Note we can replace {!CamlinternalMod} completely, but it is not replaced 
    due to we believe this is an even low level dependency
*/

@@warning("-37") /* `Function` may be used in runtime */

type rec shape =
  | Function
  | Lazy
  | Class
  | Module(array<(shape, string)>)
  | Value(Obj.t)
/* ATTENTION: check across versions */
module Array = Caml_array_extern

@set_index external set_field: (Obj.t, string, Obj.t) => unit = ""

@get_index external get_field: (Obj.t, string) => Obj.t = ""

module type Empty = {}

/** Note that we have to provide a drop in replacement, since compiler internally will
    spit out ("CamlinternalMod".[init_mod|update_mod] unless we intercept it 
    in the lambda layer
*/
let init_mod = (loc: (string, int, int), shape: shape) => {
  let undef_module = _ => raise(Undefined_recursive_module(loc))
  let rec loop = (shape: shape, struct_: Obj.t, idx) =>
    switch shape {
    | Function => set_field(struct_, idx, Obj.magic(undef_module))
    | Lazy => set_field(struct_, idx, Obj.magic(undef_module))
    | Class =>
      set_field(
        struct_,
        idx,
        Obj.magic /* ref {!CamlinternalOO.dummy_class loc} */((
          undef_module,
          undef_module,
          undef_module,
          0,
        )),
        /* depends on dummy class representation */
      )
    | Module(comps) =>
      let v = Obj.repr(module({}: Empty))
      set_field(struct_, idx, v)
      let len = Array.length(comps)
      for i in 0 to len - 1 {
        let (shape, name) = Caml_array_extern.unsafe_get(comps, i)
        loop(shape, v, name)
      }
    | Value(v) => set_field(struct_, idx, v)
    }
  let res = Obj.repr(module({}: Empty))
  let dummy_name = "dummy"
  loop(shape, res, dummy_name)
  get_field(res, dummy_name)
}

/* Note the [shape] passed between [init_mod] and [update_mod] is always the same 
   and we assume [module] is encoded as an array
*/
let update_mod = (shape: shape, o: Obj.t, n: Obj.t): unit => {
  let rec aux = (shape: shape, o, n, parent, i) =>
    switch shape {
    | Function => set_field(parent, i, n)

    | Lazy
    | Class =>
      Caml_obj.update_dummy(o, n)
    | Module(comps) =>
      for i in 0 to Array.length(comps) - 1 {
        let (shape, name) = Caml_array_extern.unsafe_get(comps, i)
        aux(shape, get_field(o, name), get_field(n, name), o, name)
      }
    | Value(_) => ()
    }
  switch shape {
  | Module(comps) =>
    for i in 0 to Array.length(comps) - 1 {
      let (shape, name) = Caml_array_extern.unsafe_get(comps, i)
      aux(shape, get_field(o, name), get_field(n, name), o, name)
    }
  | _ => assert(false)
  }
}
