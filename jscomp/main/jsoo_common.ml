module Js = struct
  module Unsafe = struct
    type any
    external inject : 'a -> any = "%identity"
    external get : 'a -> 'b -> 'c = "caml_js_get"
    external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
    external meth_call : 'a -> string -> any array -> 'b = "caml_js_meth_call"
    external pure_js_expr : string -> 'a = "caml_pure_js_expr"
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
  external create_file : js_string t -> js_string t -> unit = "caml_create_file"
  external to_bytestring : js_string t -> string = "caml_js_to_byte_string"
end

module Console = struct
  type +'a meth
  class type t =
    object
      method log : _ -> unit meth

      method log_2 : _ -> _ -> unit meth
    end

  external get_console : unit -> t Js.t = "caml_js_get_console"
  let console = get_console ()
  let log anything = (
       fun (type res a2 a1) ->
       fun (a2 : a2 Js.t)  ->
       fun (a1 : a1)  ->
       fun (_ : a2 -> a1 -> res meth)  ->
         (Js.Unsafe.meth_call a2 "log"
            [|(Js.Unsafe.inject a1)
            |] : res)
     )
       console
       anything
       (fun x  -> x#log)

end

let mk_js_error (loc: Location.t) (msg: string) = 
  let (file,line,startchar) = Location.get_pos_info loc.Location.loc_start in
  let (file,endline,endchar) = Location.get_pos_info loc.Location.loc_end in
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
  