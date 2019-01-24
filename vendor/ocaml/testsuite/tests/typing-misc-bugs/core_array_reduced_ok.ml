type sexp = A of string | L of sexp list
type 'a t = 'a array
let _ = fun (_ : 'a t)  -> ()

let array_of_sexp _ _ = [| |]
let sexp_of_array _ _ = A "foo"
let sexp_of_int _ = A "42"
let int_of_sexp _ = 42

let t_of_sexp : 'a . (sexp -> 'a) -> sexp -> 'a t=
  let _tp_loc = "core_array.ml.t" in
  fun _of_a  -> fun t  -> (array_of_sexp _of_a) t
let _ = t_of_sexp
let sexp_of_t : 'a . ('a -> sexp) -> 'a t -> sexp=
  fun _of_a  -> fun v  -> (sexp_of_array _of_a) v
let _ = sexp_of_t
module T =
  struct
    module Int =
      struct
        type t_ = int array
        let _ = fun (_ : t_)  -> ()

        let t__of_sexp: sexp -> t_ =
          let _tp_loc = "core_array.ml.T.Int.t_" in
          fun t  -> (array_of_sexp int_of_sexp) t
        let _ = t__of_sexp
        let sexp_of_t_: t_ -> sexp =
          fun v  -> (sexp_of_array sexp_of_int) v
        let _ = sexp_of_t_
      end
  end
module type Permissioned  =
  sig
    type ('a,-'perms) t
  end
module Permissioned :
  sig
    type ('a,-'perms) t
    include
      sig
        val t_of_sexp :
          (sexp -> 'a) ->
            (sexp -> 'perms) -> sexp -> ('a,'perms) t
        val sexp_of_t :
          ('a -> sexp) ->
            ('perms -> sexp) -> ('a,'perms) t -> sexp
      end
    module Int :
    sig
      type nonrec -'perms t = (int,'perms) t
      include
        sig
          val t_of_sexp :
            (sexp -> 'perms) -> sexp -> 'perms t
          val sexp_of_t :
            ('perms -> sexp) -> 'perms t -> sexp
        end
    end
  end =
  struct
    type ('a,-'perms) t = 'a array
    let _ = fun (_ : ('a,'perms) t)  -> ()

    let t_of_sexp :
      'a 'perms .
        (sexp -> 'a) ->
          (sexp -> 'perms) -> sexp -> ('a,'perms) t=
      let _tp_loc = "core_array.ml.Permissioned.t" in
      fun _of_a  -> fun _of_perms  -> fun t  -> (array_of_sexp _of_a) t
    let _ = t_of_sexp
    let sexp_of_t :
      'a 'perms .
        ('a -> sexp) ->
          ('perms -> sexp) -> ('a,'perms) t -> sexp=
      fun _of_a  -> fun _of_perms  -> fun v  -> (sexp_of_array _of_a) v
    let _ = sexp_of_t
    module Int =
      struct
        include T.Int
        type -'perms t = t_
        let _ = fun (_ : 'perms t)  -> ()

        let t_of_sexp :
          'perms . (sexp -> 'perms) -> sexp -> 'perms t=
          let _tp_loc = "core_array.ml.Permissioned.Int.t" in
          fun _of_perms  -> fun t  -> t__of_sexp t
        let _ = t_of_sexp
        let sexp_of_t :
          'perms . ('perms -> sexp) -> 'perms t -> sexp=
          fun _of_perms  -> fun v  -> sexp_of_t_ v
        let _ = sexp_of_t
      end
  end
