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

type t = Obj.t

module O = {
  @val external isArray: 'a => bool = "Array.isArray"
  type key = string
  let for_in: (Obj.t, key => unit) => unit = %raw(`function(o,foo){
        for (var x in o) { foo(x) }}
      `)

  @scope(("Object", "prototype", "hasOwnProperty"))
  @val
  /**
     JS objects are not guaranteed to have `Object` in their prototype
     chain so calling `some_obj.hasOwnProperty(key)` can sometimes throw
     an exception when dealing with JS interop. This mainly occurs when
     objects are created via `Object.create(null)`. The only safe way
     to call this function is directly, e.g. `Object.prototype.hasOwnProperty.call(some_obj, key)`.
  */
  external hasOwnProperty: (t, key) => bool = "call"

  @get_index external get_value: (Obj.t, key) => Obj.t = ""
}

/**
   Since now we change it back to use
   Array representation
   this function is higly dependent
   on how objects are encoded in buckle.

   There are potentially some issues with wrong implementation of
   `obj_dup`, for example, people call `Obj.dup` for a record,
   and new record, since currently, `new record` will generate a
   `slice` function (which assume the record is an array), and the
   output is no longer an array. (it might be  something like { 0 : x , 1 : y} )

   {[
     let u : record = Obj.dup x in
     let h = {u with x = 3}
   ]}

   ==>

   {[
     var u = obj_dup (x)
       var new_record = u.slice ()

   ]}
   `obj_dup` is a superset of `array_dup`
*/
let obj_dup: Obj.t => Obj.t = %raw(`function(x){
  if(Array.isArray(x)){
    var len = x.length  
    var v = new Array(len)
    for(var i = 0 ; i < len ; ++i){
      v[i] = x[i]
    }
    if(x.TAG !== undefined){
      v.TAG = x.TAG // TODO this can be removed eventually
    }  
    return v 
  } 
  return Object.assign({},x)    
}`)

/** 
   For the empty dummy object, whether it's 
   [[]] or [{}] depends on how 
   runtime encoding works, and will affect 
   js polymorphic comparison(Js.(=)) (fine with caml polymoprhic comparison (Pervasives.equal))
   In most cases, rec value comes from record/modules, 
   whose tag is 0, we optimize that case
*/
let update_dummy: (_, _) => unit = %raw(`function(x,y){
  var k  
  if(Array.isArray(y)){
    for(k = 0; k < y.length ; ++k){
      x[k] = y[k]
    }
    if(y.TAG !== undefined){
      x.TAG = y.TAG
    }
  } else {
    for (var k in y){
      x[k] = y[k]
    }
  }
}
`)

/** TODO: investigate total
    [compare x y] returns [0] if [x] is equal to [y],
    a negative integer if [x] is less than [y],
    and a positive integer if [x] is greater than [y].
    The ordering implemented by compare is compatible with the comparison
    predicates [=], [<] and [>] defined above, with one difference on the treatment of the float value
    [nan].

    Namely, the comparison predicates treat nan as different from any other float value,
    including itself; while compare treats [nan] as equal to itself and less than any other float value.
    This treatment of [nan] ensures that compare defines a total ordering relation.
    compare applied to functional values may raise Invalid_argument. compare applied to cyclic structures
    may not terminate.

    The compare function can be used as the comparison function required by the [Set.Make] and [Map.Make] functors,
    as well as the [List.sort] and [Array.sort] functions.
*/
let rec compare = (a: Obj.t, b: Obj.t): int =>
  if a === b {
    0
  } else {
    /* front and formoest, we do not compare function values */
    let a_type = Js.typeof(a)
    let b_type = Js.typeof(b)
    switch (a_type, b_type) {
    | ("undefined", _) => -1
    | (_, "undefined") => 1
    /* [a] is of type string, b can not be None,
        [a] could be (Some (Some x)) in that case [b] could be [Some None] or [null]
         so [b] has to be of type string or null */
    | ("string", "string") => Pervasives.compare((Obj.magic(a): string), Obj.magic(b))
    | ("string", _) => /* [b] could be [Some None] or [null] */
      1
    | (_, "string") => -1
    | ("boolean", "boolean") => Pervasives.compare((Obj.magic(a): bool), Obj.magic(b))
    | ("boolean", _) => 1
    | (_, "boolean") => -1
    | ("function", "function") => raise(Invalid_argument("compare: functional value"))
    | ("function", _) => 1
    | (_, "function") => -1
    | ("bigint", "bigint")
    | ("number", "number") =>
      Pervasives.compare((Obj.magic(a): float), (Obj.magic(b): float))
    | ("number", _) =>
      if b === Obj.repr(Js.null) || Caml_option.isNested(b) {
        1
      } else {
        /* Some (Some ..) < x */
        -1
      } /* Integer < Block in OCaml runtime GPR #1195, except Some.. */
    | (_, "number") =>
      if a === Obj.repr(Js.null) || Caml_option.isNested(a) {
        -1
      } else {
        1
      }
    | _ =>
      if a === Obj.repr(Js.null) {
        /* [b] could not be null otherwise would equal */
        if Caml_option.isNested(b) {
          1
        } else {
          -1
        }
      } else if b === Obj.repr(Js.null) {
        if Caml_option.isNested(a) {
          -1
        } else {
          1
        }
      } else if (
        /* double_array_tag: 254
         */
        Caml_option.isNested(a)
      ) {
        if Caml_option.isNested(b) {
          aux_obj_compare(a, b)
        } else {
          /* Some None < Some (Some None)) */

          /* b could not be undefined/None */
          /* Some None < Some .. */
          -1
        }
      } else {
        let tag_a = Obj.tag(a)
        let tag_b = Obj.tag(b)
        if tag_a != tag_b {
          if tag_a < tag_b {
            -1
          } else {
            1
          }
        } else {
          let len_a = Obj.size(a)
          let len_b = Obj.size(b)
          if len_a == len_b {
            if O.isArray(a) {
              aux_same_length((Obj.magic(a): array<Obj.t>), (Obj.magic(b): array<Obj.t>), 0, len_a)
            } else if %raw(`a instanceof Date && b instanceof Date`) {
              %raw(`a - b`)
            } else {
              aux_obj_compare(a, b)
            }
          } else if len_a < len_b {
            /* at least one is not zero, so it is an array block */
            aux_length_a_short((Obj.magic(a): array<Obj.t>), (Obj.magic(b): array<Obj.t>), 0, len_a)
          } else {
            aux_length_b_short((Obj.magic(a): array<Obj.t>), (Obj.magic(b): array<Obj.t>), 0, len_b)
          }
        }
      }
    }
  }

and aux_same_length = (a: array<Obj.t>, b: array<Obj.t>, i, same_length) =>
  if i == same_length {
    0
  } else {
    let res = compare(Caml_array_extern.unsafe_get(a, i), Caml_array_extern.unsafe_get(b, i))

    if res != 0 {
      res
    } else {
      aux_same_length(a, b, i + 1, same_length)
    }
  }

and aux_length_a_short = (a: array<Obj.t>, b: array<Obj.t>, i, short_length) =>
  if i == short_length {
    -1
  } else {
    let res = compare(Caml_array_extern.unsafe_get(a, i), Caml_array_extern.unsafe_get(b, i))

    if res != 0 {
      res
    } else {
      aux_length_a_short(a, b, i + 1, short_length)
    }
  }

and aux_length_b_short = (a: array<Obj.t>, b: array<Obj.t>, i, short_length) =>
  if i == short_length {
    1
  } else {
    let res = compare(Caml_array_extern.unsafe_get(a, i), Caml_array_extern.unsafe_get(b, i))

    if res != 0 {
      res
    } else {
      aux_length_b_short(a, b, i + 1, short_length)
    }
  }

and aux_obj_compare = (a: Obj.t, b: Obj.t) => {
  let min_key_lhs = ref(None)
  let min_key_rhs = ref(None)
  let do_key = ((a, b, min_key), key) =>
    if !O.hasOwnProperty(b, key) || compare(O.get_value(a, key), O.get_value(b, key)) > 0 {
      switch min_key.contents {
      | None => min_key.contents = Some(key)
      | Some(mk) =>
        if key < mk {
          min_key.contents = Some(key)
        }
      }
    }

  let do_key_a = key => do_key((a, b, min_key_rhs), key)
  let do_key_b = key => do_key((b, a, min_key_lhs), key)
  O.for_in(a, do_key_a)
  O.for_in(b, do_key_b)
  let res = switch (min_key_lhs.contents, min_key_rhs.contents) {
  | (None, None) => 0
  | (Some(_), None) => -1
  | (None, Some(_)) => 1
  | (Some(x), Some(y)) => Pervasives.compare(x, y)
  }

  res
}

type eq = (Obj.t, Obj.t) => bool

/** It is easier to do equality check than comparision, since as long as its
    basic type is not the same, it will not equal 
*/
let rec equal = (a: Obj.t, b: Obj.t): bool =>
  if a === b {
    true
  } else {
    let a_type = Js.typeof(a)
    if a_type !== "object" || a === %raw(`null`) {
      false
    } else {
      let b_type = Js.typeof(b)
      if b_type !== "object" || b === %raw(`null`) {
        false
      } else {
        /* [a] [b] could not be null, so it can not raise */
        let tag_a = Obj.tag(a)
        let tag_b = Obj.tag(b)
        if tag_a !== tag_b {
          false
        } else if O.isArray(a) {
          let len_a = Obj.size(a)
          let len_b = Obj.size(b)
          if len_a !== len_b {
            false
          } else {
            aux_equal_length((Obj.magic(a): array<Obj.t>), (Obj.magic(b): array<Obj.t>), 0, len_a)
          }
        } else if %raw(`a instanceof Error`) {
          let a: {..} = Obj.magic(a)
          let b: {..} = Obj.magic(b)
          if %raw(`b instanceof Error`) && a["message"] === b["message"] {
            equal(a["clause"], b["clause"])
          } else {
            false
          }
        } else if %raw(`a instanceof Date`) {
          if %raw(`b instanceof Date`) {
            !(Js.unsafe_gt(a, b) || Js.unsafe_lt(a, b))
          } else {
            false
          }
        } else {
          aux_obj_equal(a, b)
        }
      }
    }
  }

and aux_equal_length = (a: array<Obj.t>, b: array<Obj.t>, i, same_length) =>
  if i == same_length {
    true
  } else {
    equal(Caml_array_extern.unsafe_get(a, i), Caml_array_extern.unsafe_get(b, i)) &&
    aux_equal_length(a, b, i + 1, same_length)
  }

and aux_obj_equal = (a: Obj.t, b: Obj.t) => {
  let result = ref(true)
  let do_key_a = key =>
    if !O.hasOwnProperty(b, key) {
      result.contents = false
    }

  let do_key_b = key =>
    if !O.hasOwnProperty(a, key) || !equal(O.get_value(b, key), O.get_value(a, key)) {
      result.contents = false
    }

  O.for_in(a, do_key_a)
  if result.contents {
    O.for_in(b, do_key_b)
  }
  result.contents
}

let equal_null = (x: Obj.t, y: Js.null<Obj.t>) =>
  switch Js.nullToOption(y) {
  | None => x === Obj.magic(y)
  | Some(y) => equal(x, y)
  }

let equal_undefined = (x: Obj.t, y: Js.undefined<Obj.t>) =>
  switch Js.undefinedToOption(y) {
  | None => x === Obj.magic(y)
  | Some(y) => equal(x, y)
  }

let equal_nullable = (x: Obj.t, y: Js.nullable<Obj.t>) =>
  switch Js.toOption(y) {
  | None => x === Obj.magic(y)
  | Some(y) => equal(x, y)
  }

@inline
let isNumberOrBigInt = a => Js.typeof(a) == "number" || Js.typeof(a) == "bigint"

@inline
let canNumericCompare = (a, b) => isNumberOrBigInt(a) && isNumberOrBigInt(b)

let notequal = (a, b) =>
  if canNumericCompare(a, b) {
    (Obj.magic(a): float) != (Obj.magic(b): float)
  } else {
    !equal(a, b)
  }

let greaterequal = (a, b) =>
  if canNumericCompare(a, b) {
    (Obj.magic(a): float) >= (Obj.magic(b): float)
  } else {
    compare(a, b) >= 0
  }

let greaterthan = (a, b) =>
  if canNumericCompare(a, b) {
    (Obj.magic(a): float) > (Obj.magic(b): float)
  } else {
    compare(a, b) > 0
  }

let lessequal = (a, b) =>
  if canNumericCompare(a, b) {
    (Obj.magic(a): float) <= (Obj.magic(b): float)
  } else {
    compare(a, b) <= 0
  }

let lessthan = (a, b) =>
  if canNumericCompare(a, b) {
    (Obj.magic(a): float) < (Obj.magic(b): float)
  } else {
    compare(a, b) < 0
  }

let min = (x: Obj.t, y) =>
  if compare(x, y) <= 0 {
    x
  } else {
    y
  }

let max = (x: Obj.t, y) =>
  if compare(x, y) >= 0 {
    x
  } else {
    y
  }
