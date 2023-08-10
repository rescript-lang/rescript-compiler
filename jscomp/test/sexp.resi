/*** {1 Handling S-expressions}

    @since 0.4

    @since 0.7
    Moved the streaming parser to CCSexpStream
*/

/*** {2 Basics} */

type rec t = [
  | #Atom(string)
  | #List(list<t>)
]

let equal: (t, t) => bool
let compare: (t, t) => int
let hash: t => int

/** Build an atom directly from a string */
let atom: string => t

let of_int: int => t
let of_bool: bool => t
let of_list: list<t> => t
/** Reverse the list */
let of_rev_list: list<t> => t
/** Reverse the list */
let of_float: float => t
let of_unit: t
let of_pair: ((t, t)) => t
let of_triple: ((t, t, t)) => t
let of_quad: ((t, t, t, t)) => t

/** [of_variant name args] is used to encode algebraic variants
    into a S-expr. For instance [of_variant \"some\" [of_int 1]]
    represents the value [Some 1] */
let of_variant: (string, list<t>) => t

/** Used to represent one record field */
let of_field: (string, t) => t

/** Represent a record by its named fields */
let of_record: list<(string, t)> => t

/* {6 Traversal of S-exp}

    Example: serializing 2D points
    {[
      type pt = {x:int; y:int };;

      let pt_of_sexp e =
        Sexp.Traverse.(
          field \"x\" to_int e >>= fun x ->
          field \"y\" to_int e >>= fun y ->
          return {x;y}
        );;

      let sexp_of_pt pt = Sexp.(of_record [\"x\", of_int pt.x; \"y\", of_int pt.y]);;

      let l = [{x=1;y=1}; {x=2;y=10}];;

      let sexp = Sexp.(of_list (List.map sexp_of_pt l));;

      Sexp.Traverse.list_all pt_of_sexp sexp;;
    ]}

*/

module Traverse: {
  /** A converter from S-expressions to 'a is a function [sexp -> 'a option].
      @since 0.4.1 */
  type conv<'a> = t => option<'a>

  /** Map over a list, failing as soon as the function fails on any element
      @since 0.4.1 */
  let map_opt: ('a => option<'b>, list<'a>) => option<list<'b>>

  /** [list_any f (List l)] tries [f x] for every element [x] in [List l],
      and returns the first non-None result (if any). */
  let list_any: (conv<'a>, t) => option<'a>

  /** [list_all f (List l)] returns the list of all [y] such that [x] in [l]
      and [f x = Some y] */
  let list_all: (conv<'a>, t) => list<'a>

  /** Expect an integer */
  let to_int: conv<int>

  /** Expect a string (an atom) */
  let to_string: conv<string>

  /** Expect a boolean */
  let to_bool: conv<bool>

  /** Expect a float */
  let to_float: conv<float>

  /** Expect a list */
  let to_list: conv<list<t>>

  /** Expect a list, applies [f] to all the elements of the list, and succeeds
      only if [f] succeeded on every element
      @since 0.4.1 */
  let to_list_with: (t => option<'a>) => conv<list<'a>>

  /** Expect a list of two elements */
  let to_pair: conv<(t, t)>

  /** Same as {!to_pair} but applies conversion functions
      @since 0.4.1 */
  let to_pair_with: (conv<'a>, conv<'b>) => conv<('a, 'b)>

  let to_triple: conv<(t, t, t)>

  let to_triple_with: (conv<'a>, conv<'b>, conv<'c>) => conv<('a, 'b, 'c)>
  /* @since 0.4.1 */

  /** [get_field name e], when [e = List [(n1,x1); (n2,x2) ... ]], extracts
      the [xi] such that [name = ni], if it can find it. */
  let get_field: string => conv<t>

  /** Enriched version of {!get_field}, with a converter as argument */
  let field: (string, conv<'a>) => conv<'a>

  /** [get_variant l e] checks whether [e = List (Atom s :: args)], and
      if some pair of [l] is [s, f]. In this case, it calls [f args]
      and returns its result, otherwise it returns None. */
  let get_variant: list<(string, list<t> => option<'a>)> => conv<'a>

  /** [field_list name f  \"(... (name a b c d) ...record)\"]
      will look for a field based on the given [name], and expect it to have a
      list of arguments dealt with by [f] (here, \"a b c d\").
      @since 0.4.1 */
  let field_list: (string, list<t> => option<'a>) => conv<'a>

  let \">>=": (option<'a>, 'a => option<'b>) => option<'b>

  let \">|=": (option<'a>, 'a => 'b) => option<'b>

  let return: 'a => option<'a>

  /** Unwrap an option, possibly failing.
      @raise Invalid_argument if the argument is [None] */
  let get_exn: option<'a> => 'a
}
