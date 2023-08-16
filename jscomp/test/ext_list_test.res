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

let rec filter_map = (f: 'a => option<'b>, xs) =>
  switch xs {
  | list{} => list{}
  | list{y, ...ys} =>
    switch f(y) {
    | None => filter_map(f, ys)
    | Some(z) => list{z, ...filter_map(f, ys)}
    }
  }

let excludes = (p: 'a => bool, l): (bool, list<'a>) => {
  let excluded = ref(false)
  let rec aux = (accu, x) =>
    switch x {
    | list{} => List.rev(accu)
    | list{x, ...l} =>
      if p(x) {
        excluded := true
        aux(accu, l)
      } else {
        aux(list{x, ...accu}, l)
      }
    }
  let v = aux(list{}, l)
  if excluded.contents {
    (true, v)
  } else {
    (false, l)
  }
}

let exclude_with_fact = (p, l) => {
  let excluded = ref(None)
  let rec aux = (accu, x) =>
    switch x {
    | list{} => List.rev(accu)
    | list{x, ...l} =>
      if p(x) {
        excluded := Some(x)
        aux(accu, l)
      } else {
        aux(list{x, ...accu}, l)
      }
    }
  let v = aux(list{}, l)
  (
    excluded.contents,
    if excluded.contents != None {
      v
    } else {
      l
    },
  )
}

/** Make sure [p2 x] and [p1 x] will not hold at the same time */
let exclude_with_fact2 = (p1, p2, l) => {
  let excluded1 = ref(None)
  let excluded2 = ref(None)
  let rec aux = (accu, x) =>
    switch x {
    | list{} => List.rev(accu)
    | list{x, ...l} =>
      if p1(x) {
        excluded1 := Some(x)
        aux(accu, l)
      } else if p2(x) {
        excluded2 := Some(x)
        aux(accu, l)
      } else {
        aux(list{x, ...accu}, l)
      }
    }
  let v = aux(list{}, l)
  (
    excluded1.contents,
    excluded2.contents,
    if excluded1.contents != None && excluded2.contents != None {
      v
    } else {
      l
    },
  )
}

let rec same_length = (xs, ys) =>
  switch (xs, ys) {
  | (list{}, list{}) => true
  | (list{_, ...xs}, list{_, ...ys}) => same_length(xs, ys)
  | (_, _) => false
  }

let filter_mapi = (f: (int, 'a) => option<'b>, xs) => {
  let rec aux = (i, xs) =>
    switch xs {
    | list{} => list{}
    | list{y, ...ys} =>
      switch f(i, y) {
      | None => aux(i + 1, ys)
      | Some(z) => list{z, ...aux(i + 1, ys)}
      }
    }
  aux(0, xs)
}

let rec filter_map2 = (f: ('a, 'b) => option<'c>, xs, ys) =>
  switch (xs, ys) {
  | (list{}, list{}) => list{}
  | (list{u, ...us}, list{v, ...vs}) =>
    switch f(u, v) {
    | None => filter_map2(f, us, vs) /* idea: rec f us vs instead? */
    | Some(z) => list{z, ...filter_map2(f, us, vs)}
    }
  | _ => invalid_arg("Ext_list_test.filter_map2")
  }

let filter_map2i = (f: (int, 'a, 'b) => option<'c>, xs, ys) => {
  let rec aux = (i, xs, ys) =>
    switch (xs, ys) {
    | (list{}, list{}) => list{}
    | (list{u, ...us}, list{v, ...vs}) =>
      switch f(i, u, v) {
      | None => aux(i + 1, us, vs) /* idea: rec f us vs instead? */
      | Some(z) => list{z, ...aux(i + 1, us, vs)}
      }
    | _ => invalid_arg("Ext_list_test.filter_map2i")
    }
  aux(0, xs, ys)
}

let rec rev_map_append = (f, l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{a, ...l} => rev_map_append(f, l, list{f(a), ...l2})
  }

let flat_map2 = (f, lx, ly) => {
  let rec aux = (acc, lx, ly) =>
    switch (lx, ly) {
    | (list{}, list{}) => List.rev(acc)
    | (list{x, ...xs}, list{y, ...ys}) => aux(List.rev_append(f(x, y), acc), xs, ys)
    | (_, _) => invalid_arg("Ext_list_test.flat_map2")
    }
  aux(list{}, lx, ly)
}

let rec flat_map_aux = (f, acc, append, lx) =>
  switch lx {
  | list{} => List.rev_append(acc, append)
  | list{y, ...ys} => flat_map_aux(f, List.rev_append(f(y), acc), append, ys)
  }

let flat_map = (f, lx) => flat_map_aux(f, list{}, list{}, lx)

let flat_map_acc = (f, append, lx) => flat_map_aux(f, list{}, append, lx)

let rec map2_last = (f, l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => list{}
  | (list{u}, list{v}) => list{f(true, u, v)}
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    let r = f(false, a1, a2)
    list{r, ...map2_last(f, l1, l2)}
  | (_, _) => invalid_arg("List.map2_last")
  }

let rec map_last = (f, l1) =>
  switch l1 {
  | list{} => list{}
  | list{u} => list{f(true, u)}
  | list{a1, ...l1} =>
    let r = f(false, a1)
    list{r, ...map_last(f, l1)}
  }

let rec fold_right2_last = (f, l1, l2, accu) =>
  switch (l1, l2) {
  | (list{}, list{}) => accu
  | (list{last1}, list{last2}) => f(true, last1, last2, accu)
  | (list{a1, ...l1}, list{a2, ...l2}) => f(false, a1, a2, fold_right2_last(f, l1, l2, accu))
  | (_, _) => invalid_arg("List.fold_right2")
  }

let init = (n, f) => Array.to_list(Array.init(n, f))

let take = (n, l) => {
  let arr = Array.of_list(l)
  let arr_length = Array.length(arr)
  if arr_length < n {
    invalid_arg("Ext_list_test.take")
  } else {
    (Array.to_list(Array.sub(arr, 0, n)), Array.to_list(Array.sub(arr, n, arr_length - n)))
  }
}

let try_take = (n, l) => {
  let arr = Array.of_list(l)
  let arr_length = Array.length(arr)
  if arr_length <= n {
    (l, arr_length, list{})
  } else {
    (Array.to_list(Array.sub(arr, 0, n)), n, Array.to_list(Array.sub(arr, n, arr_length - n)))
  }
}

/**

  {[length xs = length ys + n ]}
*/
let rec length_compare = (l, n) =>
  if n < 0 {
    #Gt
  } else {
    switch l {
    | list{_, ...xs} => length_compare(xs, n - 1)
    | list{} =>
      if n == 0 {
        #Eq
      } else {
        #Lt
      }
    }
  }

/**

  {[length xs = length ys + n ]}
*/
let rec length_larger_than_n = (n, xs, ys) =>
  switch (xs, ys) {
  | (_, list{}) => length_compare(xs, n) == #Eq
  | (list{_, ...xs}, list{_, ...ys}) => length_larger_than_n(n, xs, ys)
  | (list{}, _) => false
  }

let exclude_tail = (x: list<'a>) => {
  let rec aux = (acc, x) =>
    switch x {
    | list{} => invalid_arg("Ext_list_test.exclude_tail")
    | list{x} => (x, List.rev(acc))
    | list{y0, ...ys} => aux(list{y0, ...acc}, ys)
    }
  aux(list{}, x)
}

/* For small list, only need partial equality 
   {[
     group (=) [1;2;3;4;3]
     ;;
     - : int list list = [[3; 3]; [4]; [2]; [1]]
                         # group (=) [];;
     - : 'a list list = []
   ]}
*/
let rec group = (cmp: ('a, 'a) => bool, lst: list<'a>): list<list<'a>> =>
  switch lst {
  | list{} => list{}
  | list{x, ...xs} => aux(cmp, x, group(cmp, xs))
  }

and aux = (cmp, x: 'a, xss: list<list<'a>>): list<list<'a>> =>
  switch xss {
  | list{} => list{list{x}}
  | list{y, ...ys} =>
    if cmp(x, List.hd(y)) /* cannot be null */ {
      list{list{x, ...y}, ...ys}
    } else {
      list{y, ...aux(cmp, x, ys)}
    }
  }

let stable_group = (cmp, lst) => group(cmp, lst) |> List.rev

let rec drop = (n, h) =>
  if n < 0 {
    invalid_arg("Ext_list_test.drop")
  } else if n == 0 {
    h
  } else if h == list{} {
    invalid_arg("Ext_list_test.drop")
  } else {
    drop(n - 1, List.tl(h))
  }

let rec find_first_not = (p, x) =>
  switch x {
  | list{} => None
  | list{a, ...l} =>
    if p(a) {
      find_first_not(p, l)
    } else {
      Some(a)
    }
  }

let rec for_all_opt = (p, x) =>
  switch x {
  | list{} => None
  | list{a, ...l} =>
    switch p(a) {
    | None => for_all_opt(p, l)
    | v => v
    }
  }

let fold = (f, l, init) => List.fold_left((acc, i) => f(i, init), init, l)

let rev_map_acc = (acc, f, l) => {
  let rec rmap_f = (accu, x) =>
    switch x {
    | list{} => accu
    | list{a, ...l} => rmap_f(list{f(a), ...accu}, l)
    }

  rmap_f(acc, l)
}

let rec map_acc = (acc, f, l) =>
  switch l {
  | list{} => acc
  | list{h, ...hs} => list{f(h), ...map_acc(acc, f, hs)}
  }

let rec rev_iter = (f, xs) =>
  switch xs {
  | list{} => ()
  | list{y, ...ys} =>
    rev_iter(f, ys)
    f(y)
  }

let rec for_all2_no_exn = (p, l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => true
  | (list{a1, ...l1}, list{a2, ...l2}) => p(a1, a2) && for_all2_no_exn(p, l1, l2)
  | (_, _) => false
  }

let rec find_no_exn = (p, x) =>
  switch x {
  | list{} => None
  | list{x, ...l} =>
    if p(x) {
      Some(x)
    } else {
      find_no_exn(p, l)
    }
  }

let rec find_opt = (p, x) =>
  switch x {
  | list{} => None
  | list{x, ...l} =>
    switch p(x) {
    | Some(_) as v => v
    | None => find_opt(p, l)
    }
  }

let split_map = (f: 'a => ('b, 'c), xs: list<'a>): (list<'b>, list<'c>) => {
  let rec aux = (bs, cs, xs) =>
    switch xs {
    | list{} => (List.rev(bs), List.rev(cs))
    | list{u, ...us} =>
      let (b, c) = f(u)
      aux(list{b, ...bs}, list{c, ...cs}, us)
    }

  aux(list{}, list{}, xs)
}

/*
   {[
     reduce_from_right (-) [1;2;3];;
     - : int = 2
               # reduce_from_right (-) [1;2;3; 4];;
     - : int = -2
                # reduce_from_right (-) [1];;
     - : int = 1
               # reduce_from_right (-) [1;2;3; 4; 5];;
     - : int = 3
   ]} 
*/
let reduce_from_right = (fn, lst) =>
  switch List.rev(lst) {
  | list{last, ...rest} => List.fold_left((x, y) => fn(y, x), last, rest)
  | _ => invalid_arg("Ext_list_test.reduce")
  }
let reduce_from_left = (fn, lst) =>
  switch lst {
  | list{first, ...rest} => List.fold_left(fn, first, rest)
  | _ => invalid_arg("Ext_list_test.reduce_from_left")
  }

type t<'a> = ref<list<'a>>

let create_ref_empty = () => ref(list{})

let ref_top = x =>
  switch x.contents {
  | list{y, ..._} => y
  | _ => invalid_arg("Ext_list_test.ref_top")
  }

let ref_empty = x =>
  switch x.contents {
  | list{} => true
  | _ => false
  }

let ref_push = (x, refs) => refs := list{x, ...refs.contents}

let ref_pop = refs =>
  switch refs.contents {
  | list{} => invalid_arg("Ext_list_test.ref_pop")
  | list{x, ...rest} =>
    refs := rest
    x
  }

let rev_except_last = xs => {
  let rec aux = (acc, xs) =>
    switch xs {
    | list{} => invalid_arg("Ext_list_test.rev_except_last")
    | list{x} => (acc, x)
    | list{x, ...xs} => aux(list{x, ...acc}, xs)
    }
  aux(list{}, xs)
}

let sort_via_array = (cmp, lst) => {
  let arr = Array.of_list(lst)
  Array.sort(cmp, arr)
  Array.to_list(arr)
}

let rec last = xs =>
  switch xs {
  | list{x} => x
  | list{_, ...tl} => last(tl)
  | list{} => invalid_arg("Ext_list_test.last")
  }

let rec assoc_by_string = (def, k: string, lst) =>
  switch lst {
  | list{} =>
    switch def {
    | None => assert(false)
    | Some(x) => x
    }
  | list{(k1, v1), ...rest} =>
    if Ext_string_test.equal(k1, k) {
      v1
    } else {
      assoc_by_string(def, k, rest)
    }
  }

let rec assoc_by_int = (def, k: int, lst) =>
  switch lst {
  | list{} =>
    switch def {
    | None => assert(false)
    | Some(x) => x
    }
  | list{(k1, v1), ...rest} =>
    if k1 == k {
      v1
    } else {
      assoc_by_int(def, k, rest)
    }
  }

/* `modulo [1;2;3;4] [1;2;3]` => [1;2;3], Some [4] `
  modulo [1;2;3] [1;2;3;4] => [1;2;3] None 
  modulo [1;2;3] [1;2;3] => [1;2;3] Some []
 */
 