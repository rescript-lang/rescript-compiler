(**
This module is shared between different JSOO / Playground based modules
*)
module Js :
  sig
    module Unsafe :
      sig
        type any
        external inject : 'a -> any = "%identity"
        external get : 'a -> 'b -> 'c = "caml_js_get"
        external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
        external pure_js_expr : string -> 'a = "caml_pure_js_expr"
        val global : 'a
        type obj
        external obj : (string * any) array -> obj = "caml_js_object"
      end
    type (-'a, +'b) meth_callback
    type 'a callback = (unit, 'a) meth_callback
    external wrap_callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
      = "caml_js_wrap_callback"
    external wrap_meth_callback : ('a -> 'b) -> ('a, 'b) meth_callback
      = "caml_js_wrap_meth_callback"
    type +'a t
    type js_string
    external string : string -> js_string t = "caml_js_from_string"
    external to_string : js_string t -> string = "caml_js_to_string"
    external create_file : js_string t -> js_string t -> unit
      = "caml_create_file"
    external to_bytestring : js_string t -> string = "caml_js_to_byte_string"
  end

module Console :
  sig
    type +'a meth
    class type t =
      object
        method log : _ -> unit meth
        method log_2 : _ -> _ -> unit meth
      end
    val console : t Js.t
    val log: 'a -> unit
  end

(*
Creates a Js Error object for given location with and a certain error message
*)
val mk_js_error : Location.t -> string -> Js.Unsafe.obj
