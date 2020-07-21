(*
 * This module extracts most JSOO externals from the upstream jsoo `js.ml` file.
 * See: https://github.com/ocsigen/js_of_ocaml/blob/master/lib/js_of_ocaml/js.ml
 *)
module Js = struct
  module Unsafe = struct
    type any
    external inject : 'a -> any = "%identity"
    external get : 'a -> 'b -> 'c = "caml_js_get"
    external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
    external pure_js_expr : string -> 'a = "caml_pure_js_expr"
    external fun_call : 'a -> any array -> 'b = "caml_js_fun_call"
    let global = pure_js_expr "joo_global_object"
    type obj
    external obj : (string * any) array -> obj = "caml_js_object"
  end
  type (-'a, +'b) meth_callback
  type 'a callback = (unit, 'a) meth_callback

  external wrap_callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback = "caml_js_wrap_callback"
  external wrap_meth_callback : ('a -> 'b) -> ('a, 'b) meth_callback = "caml_js_wrap_meth_callback"

  type + 'a t

  type js_string
  external string : string -> js_string t = "caml_js_from_string"
  external to_string : js_string t -> string = "caml_js_to_string"

  type 'a js_array
  external array : 'a array -> 'a js_array = "caml_js_from_array"

  external bool : bool -> bool t = "caml_js_from_bool"
  external to_bool : bool t -> bool = "caml_js_to_bool"

  type number
  external number_of_float : float -> number t = "caml_js_from_float"
  external float_of_number : number t -> float = "caml_js_to_float"

  external create_file : js_string t -> js_string t -> unit = "caml_create_file"
  external to_bytestring : js_string t -> string = "caml_js_to_byte_string"
end

let mk_js_error (loc: Location.t) (msg: string) = 
  let (_file,line,startchar) = Location.get_pos_info loc.Location.loc_start in
  let (_file,endline,endchar) = Location.get_pos_info loc.Location.loc_end in
  Js.Unsafe.(obj
  [|
  "js_error_msg",
    inject @@ Js.string (Printf.sprintf "Line %d, %d:\n  %s"  line startchar msg);
  "row"    , inject (line - 1);
  "column" , inject startchar;
  "endRow" , inject (endline - 1);
  "endColumn" , inject endchar;
  "text" , inject @@ Js.string msg;
  "type" , inject @@ Js.string "error"
  |]
  )
  
