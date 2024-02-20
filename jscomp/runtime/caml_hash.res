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
@@config({flags: ["-bs-noassertfalse"]})
type rec cell<'a> = {
  content: 'a,
  mutable next: option<cell<'a>>,
}
and t<'a> = {
  mutable length: int,
  mutable first: option<cell<'a>>,
  mutable last: option<cell<'a>>,
}

let create_queue = () => {
  length: 0,
  first: None,
  last: None,
}

/* Added to tail */
let push_back = (q: t<'a>, v: 'a) => {
  let cell = Some({content: v, next: None})

  switch q.last {
  | None =>
    q.length = 1
    q.first = cell
    q.last = cell
  | Some(last) =>
    q.length = q.length + 1
    last.next = cell
    q.last = cell
  }
}

let is_empty_queue = q => q.length == 0

/* pop from front */

let unsafe_pop = (q: t<'a>) =>
  switch q.first {
  | None => assert(false)
  | Some(cell) =>
    let next = cell.next
    if next == None {
      q.length = 0
      q.first = None
      q.last = None
    } else {
      q.length = q.length - 1
      q.first = next
    }
    cell.content
  }

let {hash_mix_int, hash_final_mix, hash_mix_string} = module(Caml_hash_primitive)

let hash = (count: int, _limit, seed: int, obj: Obj.t): int => {
  let s = ref(seed)
  if Js.typeof(obj) == "number" {
    let u = Caml_nativeint_extern.of_float(Obj.magic(obj))
    s.contents = hash_mix_int(s.contents, u + u + 1)
    hash_final_mix(s.contents)
  } else if Js.typeof(obj) == "string" {
    s.contents = hash_mix_string(s.contents, (Obj.magic(obj): string))
    hash_final_mix(s.contents)
  } else {
    /* TODO: hash [null] [undefined] as well */

    let queue = create_queue()
    let num = ref(count)
    let () = {
      push_back(queue, obj)
      num.contents = num.contents - 1
    }

    while !is_empty_queue(queue) && num.contents > 0 {
      let obj = unsafe_pop(queue)
      if Js.typeof(obj) == "number" {
        let u = Caml_nativeint_extern.of_float(Obj.magic(obj))
        s.contents = hash_mix_int(s.contents, u + u + 1)
        num.contents = num.contents - 1
      } else if Js.typeof(obj) == "string" {
        s.contents = hash_mix_string(s.contents, (Obj.magic(obj): string))
        num.contents = num.contents - 1
      } else if Js.typeof(obj) == "boolean" {
        ()
      } else if Js.typeof(obj) == "undefined" {
        ()
      } else if Js.typeof(obj) == "symbol" {
        ()
      } else if Js.typeof(obj) == "function" {
        ()
      } else {
        let size = Obj.size(obj)
        if size != 0 {
          let obj_tag = Obj.tag(obj)
          let tag = lor(lsl(size, 10), obj_tag)
          if obj_tag == 248 /* Obj.object_tag */ {
            s.contents = hash_mix_int(s.contents, (Obj.obj(Obj.field(obj, 1)): int))
          } else {
            s.contents = hash_mix_int(s.contents, tag)
            let block = {
              let v = size - 1
              if v < num.contents {
                v
              } else {
                num.contents
              }
            }
            for i in 0 to block {
              push_back(queue, Obj.field(obj, i))
            }
          }
        } else {
          let size: int = %raw(`function(obj,cb){
            var size = 0  
            for(var k in obj){
              cb(obj[k])
              ++ size
            }
            return size
          }`)(.obj, (. v) => push_back(queue, v))
          s.contents = hash_mix_int(s.contents, lor(lsl(size, 10), 0)) /* tag */
        }
      }
    }
    hash_final_mix(s.contents)
  }
}
