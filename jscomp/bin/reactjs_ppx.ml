module Syntax_util
= struct
#1 "syntax_util.ml"
open Ast_mapper
open Asttypes
open Parsetree
open Longident
open Ast_helper

(** [is_prefixed prefix i str] checks if prefix is the prefix of str
  * starting from position i
  *)
let is_prefixed prefix str i =
  let len = String.length prefix in
  if i + len > String.length str then false else
  let rec loop j =
    if j >= len then true else
      if String.unsafe_get prefix j <> String.unsafe_get str (i + j) then false else loop (j + 1)
    in
  loop 0

(**
 * pick_while returns a tuple where first element is longest prefix (possibly empty) of the list of elements that satisfy p
 * and second element is the remainder of the list
 *)
let rec pick_while p = function
  | [] -> [], []
  | hd::tl when p hd ->
                  let (satisfied, not_satisfied) = pick_while p tl in
                  hd :: satisfied, not_satisfied
  | l -> ([], l)


let rec replace_string_ old_str new_str i str buffer =
  if i >= String.length str then
    ()
  else
    (* found match *)
    if is_prefixed old_str str i then
      (* split string *)
      let old_str_len = String.length old_str in
      Buffer.add_string buffer new_str;
      replace_string_ old_str new_str (i + old_str_len) str buffer
    else
      let start = String.sub str i 1 in
      Buffer.add_string buffer start;
      replace_string_ old_str new_str (i + 1) str buffer


(** [replace_string old_str new_str str] replaces old_str to new_str in str *)
let replace_string old_str new_str str =
  let buffer = Buffer.create (String.length old_str * 2) in
  replace_string_ old_str new_str 0 str buffer;
  Buffer.contents buffer

(* This is lifted from https://github.com/bloomberg/bucklescript/blob/14d94bb9c7536b4c5f1208c8e8cc715ca002853d/jscomp/ext/ext_string.ml#L32
  Thanks @bobzhang and @hhugo! *)
let split_by ?(keep_empty=false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      if last_pos = 0 && not keep_empty then
        (*
           {[ split " test_unsafe_obj_ffi_ppx.cmi" ~keep_empty:false ' ']}
        *)
        acc
      else
        String.sub str 0 last_pos :: acc
    else
      if is_delim str.[pos] then
        let new_len = (last_pos - pos - 1) in
        if new_len <> 0 || keep_empty then
          let v = String.sub str (pos + 1) new_len in
          loop ( v :: acc)
            pos (pos - 1)
        else loop acc pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let rec trim_right_idx str idx =
  if idx = -1 then 0
  else
    match String.get str idx with
    | '\t' | ' ' | '\n' | '\r' -> trim_right_idx str (idx - 1)
    | _ -> idx + 1

let trim_right str =
  let length = String.length str in
  if length = 0 then ""
  else
    let index = trim_right_idx str (length - 1) in
    if index = 0 then ""
    else if index = length then str
    else String.sub str 0 index

let strip_trailing_whitespace str =
  split_by ~keep_empty:true (fun x -> x = '\n') str
  |> List.map trim_right
  |> String.concat "\n"
  |> String.trim

module StringMap = Map.Make (String)


(** Generate a suitable extension node for Merlin's consumption,
    for the purposes of reporting a syntax error - only used
    in recovery mode.
 *)
let syntax_error_extension_node loc message =
  let str = Location.mkloc "merlin.syntax-error" loc in
  let payload = PStr [{
    pstr_loc = Location.none;
    pstr_desc = Pstr_eval (
      {
        pexp_loc = Location.none;
        pexp_desc = Pexp_constant (Asttypes.Const_string (message, None));
        pexp_attributes = [];
      },
      []
    );
  }]
 in
 (str, payload)

let reason_to_ml_swapping_alist = [
  "===",  "==";
  "==",  "=";
  (* ===\/ and !==\/ are not representable in OCaml but
   * representable in Reason
   *)
  "\\!==", "!==";
  "\\===", "===";
  "!=", "<>";
  "!==", "!=";
  "match", "switch";
  "method", "pub";
  "private", "pri";
]

let swap_txt map txt =
  if StringMap.mem txt map then
    StringMap.find txt map
  else
    txt

(** identifier_mapper maps all identifiers in an AST with a mapping function f
  *)
let identifier_mapper f =
{ default_mapper with
  expr = begin fun mapper expr ->
    let expr =
      match expr with
        | {pexp_desc=Pexp_ident ({txt} as id);
           pexp_loc;
           pexp_attributes} ->
             let swapped = match txt with
               | Lident s -> Lident (f s)
               | Ldot(longPrefix, s) -> Ldot(longPrefix, f s)
               | Lapply (y,s) -> Lapply (y, s)
             in
             {expr with pexp_desc=Pexp_ident ({id with txt=swapped})}
        | _ -> expr
    in
    default_mapper.expr mapper expr
  end;
  pat = begin fun mapper pat ->
    let pat =
      match pat with
        | {ppat_desc=Ppat_var ({txt} as id);
           ppat_loc;
           ppat_attributes} ->
             {pat with ppat_desc=Ppat_var ({id with txt=(f txt)})}
        | _ -> pat
    in
    default_mapper.pat mapper pat
  end;
}
(*
let create_auto_printer_mapper =
  let attach_printer = function
    | { pstr_desc=Pstr_type type_decls } as ty ->
        let str_of_type = Ppx_deriving_show.str_of_type ~options:[] ~path:[] in
        let printer = List.concat (List.map str_of_type type_decls) in
        (ty, Some (Str.value Recursive printer))
    | ty -> (ty, None)
  in
  { default_mapper with structure = begin fun mapper decls ->
    let decls =
      let maybe_concat acc = function
        | (s, None) -> s::acc
        | (s, Some x) -> x::s::acc
      in
      List.rev (List.fold_left maybe_concat [] (List.map attach_printer decls))
    in
    default_mapper.structure mapper decls
  end }
*)

(** unescape_stars_slashes_mapper unescapes all stars and slases in an AST
  *)
let unescape_stars_slashes_mapper =
  let unescape_stars_slashes str =
    let len = String.length str in
    if len < 2 then
      str
    else
      let ending = String.sub str 1 (len - 1) in
    String.sub str 0 1 ^
      replace_string "\\*" "*"
        (replace_string ("\\/") "/" ending)
  in
  identifier_mapper unescape_stars_slashes

(** escape_stars_slashes_mapper escapes all stars and slases in an AST
  *)
let escape_stars_slashes_mapper =
  let escape_stars_slashes str =
    let len = String.length str in
    if len < 2 then
      str
    else
      let ending = String.sub str 1 (len -1) in
      String.sub str 0 1 ^
        replace_string "*" "\\*"
          (replace_string "/" "\\/" ending)
  in
  identifier_mapper escape_stars_slashes

(**
 * swap_operator_mapper is a mapper that swaps two operators at parse/print time.
 * We need this since we want to transform operator such as "=" in Ocaml to "==" in Reason.
 * In this case, in the parser, everytime we see a token "==" in Reason, we transform it into "=";
 * Similarly, in the printer, everytime we see a token "=", we transform it into "==";
 *)
let swap_operator_mapper map = identifier_mapper (swap_txt map)

let reason_to_ml_swap_map = List.fold_left
                              (fun map (op1, op2) -> (StringMap.add op1 op2 map))
                              StringMap.empty
                              reason_to_ml_swapping_alist

let ml_to_reason_swap_map = List.fold_left
                              (fun map (op1, op2) -> (StringMap.add op2 op1 map))
                              StringMap.empty
                              reason_to_ml_swapping_alist

(* To be used in parser, transform a token into an ast node with different identifier
 *)
let reason_to_ml_swap_operator_mapper = swap_operator_mapper reason_to_ml_swap_map

(* To be used in printer, transform an ast node into a token with different identifier
 *)
let ml_to_reason_swap_operator_mapper = swap_operator_mapper ml_to_reason_swap_map

(* attribute_equals tests an attribute is txt
 *)
let attribute_equals to_compare = function
  | ({txt; _}, _) -> txt = to_compare

(* attribute_exists tests if an attribute exists in a list
 *)
let attribute_exists txt attributes = List.exists (attribute_equals txt) attributes

(* conflicted_attributes tests if both attribute1 and attribute2
 * exist
 *)
let attributes_conflicted attribute1 attribute2 attributes =
  attribute_exists attribute1 attributes &&
  attribute_exists attribute2 attributes

(* normalized_attributes removes attribute from a list of attributes
 *)
let normalized_attributes attribute attributes =
  List.filter (fun x -> not (attribute_equals attribute x)) attributes

(*
 * apply_mapper_chain family applies an ast_mapper_chain to an ast,
 * ordering from left to right.
 *)
let apply_mapper_chain_to_structure =
  List.fold_left (fun s mapper -> mapper.structure mapper s )
let apply_mapper_chain_to_signature =
  List.fold_left (fun s mapper -> mapper.signature mapper s )
let apply_mapper_chain_to_type =
  List.fold_left (fun s mapper -> mapper.typ mapper s )
let apply_mapper_chain_to_expr =
  List.fold_left (fun s mapper -> mapper.expr mapper s )
let apply_mapper_chain_to_pattern =
  List.fold_left (fun s mapper -> mapper.pat mapper s )

let apply_mapper_chain_to_toplevel_phrase toplevel_phrase chain =
  match toplevel_phrase with
  | Ptop_def x -> Ptop_def (apply_mapper_chain_to_structure x chain)
  | x -> x

let apply_mapper_chain_to_use_file use_file chain =
  List.map (fun x -> apply_mapper_chain_to_toplevel_phrase x chain) use_file

(* The following logic defines our own Error object
 * and register it with ocaml so it knows how to print it
 *)

type error = Syntax_error of string

exception Error of Location.t * error

let report_error ppf (Syntax_error err) =
  Format.(fprintf ppf "%s" err)

let () =
  Location.register_error_of_exn
    (function
     | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
     | _ ->
        None
     )


type menhirMessagesError = {
  msg: string;
  loc: Location.t;
}

type menhirError =
  | NoMenhirMessagesError
  | MenhirMessagesError of menhirMessagesError

let menhirMessagesError = ref [NoMenhirMessagesError]

let findMenhirErrorMessage loc =
    let rec find messages =
      match messages with
      | MenhirMessagesError err :: tail when err.loc = loc -> MenhirMessagesError err
      | _ :: tail -> find tail
      | [] -> NoMenhirMessagesError
    in find !menhirMessagesError

let add_error_message err =
  let msg = if err.msg = "<SYNTAX ERROR>\n" then
    [MenhirMessagesError {err with msg = "A syntax error occurred. Help to improve this message: https://github.com/facebook/reason/wiki/Add-a-Menhir-error-message"}]
  else
    [MenhirMessagesError err]
  in
  menhirMessagesError := !menhirMessagesError @ msg;

end
module Reactjs_jsx_ppx
= struct
#1 "reactjs_jsx_ppx.ml"
(* transform `div props1::a props2::b children::[foo, bar] () [@JSX]` into
  `ReactDOMRe.createElement "div" props::[%bs.obj {props1: 1, props2: b}] [|foo, bar|]`.
  Don't transform the upper-cased case: `Foo.createElement foo::bar children::[] () [@JSX]`.
*)

(* Why do we need a transform, instead of just using the original format?
Because that one currently doesn't work well for the existing React.js *)
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let rec listToArray' lst accum =
  (* not in the sense of converting a list to an array; convert the AST
    reprensentation of a list to the AST reprensentation of an array *)
  match lst with
  | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} -> accum
  | {
      pexp_desc = Pexp_construct (
        {txt = Lident "::"},
        Some {pexp_desc = Pexp_tuple (v::acc::[])}
      )
  } -> listToArray' acc (v::accum)
  | _ -> raise (
    Invalid_argument "JSX: the `children` prop must be a literal list (of react elements)."
  )

let listToArray lst = listToArray' lst [] |> List.rev

let extractChildrenForDOMElements ?(removeLastPositionUnit=false) ~loc propsAndChildren =
  let rec allButLast_ lst acc = match lst with
    | [] -> []
    | ("", {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})::[] -> acc
    | ("", _)::rest -> raise (Invalid_argument "JSX: found non-labelled argument before the last position")
    | arg::rest -> allButLast_ rest (arg::acc)
  in
  let allButLast lst = allButLast_ lst [] |> List.rev in
  match (List.partition (fun (label, expr) -> label = "children") propsAndChildren) with
  | ((label, childrenExpr)::[], props) ->
    (childrenExpr, if removeLastPositionUnit then allButLast props else props)
  | ([], props) ->
    (* no children provided? Place a placeholder list (don't forgot we're talking about DOM element conversion here only) *)
    (Exp.construct ~loc {loc; txt = Lident "[]"} None, if removeLastPositionUnit then allButLast props else props)
  | (moreThanOneChild, props) -> raise (Invalid_argument "JSX: somehow there's more than one `children` label")

(* TODO: some line number might still be wrong *)
let jsxMapper argv = {
  default_mapper with
  expr = (fun mapper expression -> match expression with
    (* spotted a function application! Does it have the @JSX attribute? *)
    | {
        pexp_desc = Pexp_apply ({pexp_desc = createElementWrap} as wrap, propsAndChildren);
        pexp_attributes
      } when Syntax_util.attribute_exists "JSX" pexp_attributes ->
        (match createElementWrap with
        | Pexp_ident caller ->
          (match caller with
          | {txt = Lident "createElement"} ->
            raise (Invalid_argument "JSX: `createElement` should be preceeded by a module name.")
          (* Foo.createElement prop1::foo prop2:bar children::[] () *)
          (* no change *)
          | {loc; txt = Ldot (moduleNames, "createElement")} ->
            let attrs = pexp_attributes |> List.filter (fun (attribute, _) -> attribute.txt <> "JSX") in
            Exp.apply
              ~loc
              ~attrs
              wrap
              (
                propsAndChildren |> List.map (fun (label, expr) -> (label, mapper.expr mapper expr))
              )
          (* div prop1::foo prop2:bar children::[bla] () *)
          (* turn that into ReactDOMRe.createElement props::(ReactDOMRe.props props1::foo props2::bar ()) [|bla|] *)
          | {loc; txt = Lident lowercaseIdentifier} ->
            let (children, propsWithLabels) =
              extractChildrenForDOMElements ~loc propsAndChildren
            in
            let componentNameExpr =
              Exp.constant ~loc (Const_string (lowercaseIdentifier, None))
            in
            let childrenExpr =
              Exp.array (
                listToArray children |> List.map (fun a -> mapper.expr mapper a)
              )
            in
            let args = match propsWithLabels with
            | [theUnitArgumentAtEnd] ->
              [
                (* "div" *)
                ("", componentNameExpr);
                (* [|moreCreateElementCallsHere|] *)
                ("", childrenExpr)
              ]
            | nonEmptyProps ->
              let propsCall =
                Exp.apply
                  ~loc
                  (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "props")})
                  (nonEmptyProps |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression)))
              in
              [
                (* "div" *)
                ("", componentNameExpr);
                (* ReactDOMRe.props className:blabla foo::bar () *)
                ("props", propsCall);
                (* [|moreCreateElementCallsHere|] *)
                ("", childrenExpr)
              ]
            in
            Exp.apply
              ~loc
              (* throw away the [@JSX] attribute and keep the others, if any *)
              ~attrs:(pexp_attributes |> List.filter (fun (attribute, _) -> attribute.txt <> "JSX"))
              (* ReactDOMRe.createDOMElement *)
              (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "createElement")})
              args
          | {txt = Ldot (_, anythingNotCreateElement)} ->
            raise (
              Invalid_argument
                ("JSX: the JSX attribute should be attached to a `YourModuleName.createElement` call. We saw `"
                  ^ anythingNotCreateElement
                  ^ "` instead"
                )
            )
          | {txt = Lapply _} ->
            (* don't think there's ever a case where this is reached *)
            raise (
              Invalid_argument "JSX: encountered a weird case while processing the code. Please report this!"
            )
          )
        | anythingElseThanIdent ->
          raise (
            Invalid_argument "JSX: `createElement` should be preceeded by a simple, direct module name."
          )
        )
    (* Delegate to the default mapper, a deep identity traversal *)
    | x -> default_mapper.expr mapper x)
}

let () = register "JSX" jsxMapper

end
