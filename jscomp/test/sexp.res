/*** {1 Simple S-expression parsing/printing} */

type rec t = [
  | #Atom(string)
  | #List(list<t>)
]

let equal = (a, b) => a == b

let compare = (a, b) => Pervasives.compare(a, b)

let hash = a => Hashtbl.hash(a)

let of_int = x => #Atom(string_of_int(x))
let of_float = x => #Atom(string_of_float(x))
let of_bool = x => #Atom(string_of_bool(x))
let atom = x => #Atom(x)
let of_unit = #List(list{})
let of_list = l => #List(l)
let of_rev_list = l => #List(List.rev(l))
let of_pair = ((x, y)) => #List(list{x, y})
let of_triple = ((x, y, z)) => #List(list{x, y, z})
let of_quad = ((x, y, z, u)) => #List(list{x, y, z, u})

let of_variant = (name, args) => #List(list{#Atom(name), ...args})
let of_field = (name, t) => #List(list{#Atom(name), t})
let of_record = l => #List(List.map(((n, x)) => of_field(n, x), l))

/*** {6 Traversal of S-exp} */

module Traverse = {
  type conv<'a> = t => option<'a>

  let return = x => Some(x)

  let \">|=" = (e, f) =>
    switch e {
    | None => None
    | Some(x) => Some(f(x))
    }

  let \">>=" = (e, f) =>
    switch e {
    | None => None
    | Some(x) => f(x)
    }

  let map_opt = (f, l) => {
    let rec recurse = (acc, l) =>
      switch l {
      | list{} => Some(List.rev(acc))
      | list{x, ...l'} =>
        switch f(x) {
        | None => None
        | Some(y) => recurse(list{y, ...acc}, l')
        }
      }
    recurse(list{}, l)
  }

  let rec _list_any = (f, l) =>
    switch l {
    | list{} => None
    | list{x, ...tl} =>
      switch f(x) {
      | Some(_) as res => res
      | None => _list_any(f, tl)
      }
    }

  let list_any = (f, e) =>
    switch e {
    | #Atom(_) => None
    | #List(l) => _list_any(f, l)
    }

  let rec _list_all = (f, acc, l) =>
    switch l {
    | list{} => List.rev(acc)
    | list{x, ...tl} =>
      switch f(x) {
      | Some(y) => _list_all(f, list{y, ...acc}, tl)
      | None => _list_all(f, acc, tl)
      }
    }

  let list_all = (f, e) =>
    switch e {
    | #Atom(_) => list{}
    | #List(l) => _list_all(f, list{}, l)
    }

  let _try_atom = (e, f) =>
    switch e {
    | #List(_) => None
    | #Atom(x) =>
      try Some(f(x)) catch {
      | _ => None
      }
    }

  let to_int = e => _try_atom(e, int_of_string)
  let to_bool = e => _try_atom(e, bool_of_string)
  let to_float = e => _try_atom(e, float_of_string)
  let to_string = e => _try_atom(e, x => x)

  let to_pair = e =>
    switch e {
    | #List(list{x, y}) => Some(x, y)
    | _ => None
    }

  let to_pair_with = (f1, f2, e) =>
    \">>="(to_pair(e), ((x, y)) => \">>="(f1(x), x => \">>="(f2(y), y => return((x, y)))))

  let to_triple = e =>
    switch e {
    | #List(list{x, y, z}) => Some(x, y, z)
    | _ => None
    }

  let to_triple_with = (f1, f2, f3, e) =>
    \">>="(to_triple(e), ((x, y, z)) =>
      \">>="(f1(x), x => \">>="(f2(y), y => \">>="(f3(z), z => return((x, y, z)))))
    )

  let to_list = e =>
    switch e {
    | #List(l) => Some(l)
    | #Atom(_) => None
    }

  let to_list_with = (f, e: t) =>
    switch e {
    | #List(l) => map_opt(f, l)
    | #Atom(_) => None
    }

  let rec _get_field = (name, l) =>
    switch l {
    | list{#List(list{#Atom(n), x}), ..._} if name == n => Some(x)
    | list{_, ...tl} => _get_field(name, tl)
    | list{} => None
    }

  let get_field = (name, e) =>
    switch e {
    | #List(l) => _get_field(name, l)
    | #Atom(_) => None
    }

  let field = (name, f, e) => \">>="(get_field(name, e), f)

  let rec _get_field_list = (name, l) =>
    switch l {
    | list{#List(list{#Atom(n), ...tl}), ..._} if name == n => Some(tl)
    | list{_, ...tl} => _get_field_list(name, tl)
    | list{} => None
    }

  let field_list = (name, f, e) =>
    switch e {
    | #List(l) => \">>="(_get_field_list(name, l), f)
    | #Atom(_) => None
    }

  let rec _get_variant = (s, args, l) =>
    switch l {
    | list{} => None
    | list{(s', f), ..._} if s == s' => f(args)
    | list{_, ...tl} => _get_variant(s, args, tl)
    }

  let get_variant = (l, e) =>
    switch e {
    | #List(list{#Atom(s), ...args}) => _get_variant(s, args, l)
    | #List(_) => None
    | #Atom(s) => _get_variant(s, list{}, l)
    }

  let get_exn = e =>
    switch e {
    | None => failwith("CCSexp.Traverse.get_exn")
    | Some(x) => x
    }
}
