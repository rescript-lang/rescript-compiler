type recordGen = {mutable unboxed: int; mutable boxed: int}
type recordValue = int
type moduleItem = string
type moduleAccessPath = Root of string | Dot of moduleAccessPath * moduleItem

let recordValueToString recordValue = recordValue |> string_of_int
let recordGen () = {unboxed = 0; boxed = 0}

let newRecordValue ~unboxed recordGen =
  if unboxed then (
    let v = recordGen.unboxed in
    recordGen.unboxed <- recordGen.unboxed + 1;
    v)
  else
    let v = recordGen.boxed in
    recordGen.boxed <- recordGen.boxed + 1;
    v

let newModuleItem ~name = name

let rec emitModuleAccessPath ~config moduleAccessPath =
  match moduleAccessPath with
  | Root s -> s
  | Dot (p, moduleItem) ->
    p |> emitModuleAccessPath ~config |> EmitText.fieldAccess ~label:moduleItem

let jsVariantTag ~polymorphic =
  match polymorphic with
  | true -> "NAME"
  | false -> "TAG"

let jsVariantPayloadTag ~n = "_" ^ string_of_int n

let jsVariantValue ~polymorphic =
  match polymorphic with
  | true -> "VAL"
  | false -> "value"

let isMutableObjectField name =
  String.length name >= 2
  && (String.sub name (String.length name - 2) 2 [@doesNotRaise]) = "#="

(** Mutable fields, i.e. fields annotated "[@bs.set]"
   are represented as extra fields called "fieldName#="
   preceding the normal field. *)
let checkMutableObjectField ~previousName ~name = previousName = name ^ "#="

let default = "$$default"

module Mangle = struct
  let keywords =
    [|
      "and";
      "as";
      "assert";
      "begin";
      "class";
      "constraint";
      "do";
      "done";
      "downto";
      "else";
      "end";
      "exception";
      "external";
      "false";
      "for";
      "fun";
      "function";
      "functor";
      "if";
      "in";
      "include";
      "inherit";
      "initializer";
      "lazy";
      "let";
      "match";
      "method";
      "module";
      "mutable";
      "new";
      "nonrec";
      "object";
      "of";
      "open";
      "or";
      "private";
      "rec";
      "sig";
      "struct";
      "then";
      "to";
      "true";
      "try";
      "type";
      "val";
      "virtual";
      "when";
      "while";
      "with";
      "mod";
      "land";
      "lor";
      "lxor";
      "lsl";
      "lsr";
      "asr";
    |]

  let table = Hashtbl.create (keywords |> Array.length);;

  keywords |> Array.iter (fun x -> Hashtbl.add table ("_" ^ x) x)

  (**
    Apply ReScript's mangling rules for object field names:
    Remove trailing "__" if present.
    Otherwise remove leading "_" when followed by an uppercase letter, or keyword.
  *)
  let translate x =
    let len = x |> String.length in
    if
      len > 2
      && (x.[len - 1] [@doesNotRaise]) = '_'
      && (x.[len - 2] [@doesNotRaise]) = '_'
    then (* "foo__" -> "foo" *) String.sub x 0 (len - 2) [@doesNotRaise]
    else if len > 1 && (x.[0] [@doesNotRaise]) = '_' then
      if (x.[1] [@doesNotRaise]) >= 'A' && (x.[1] [@doesNotRaise]) <= 'Z' then
        (* "_Uppercase" => "Uppercase"s *)
        String.sub x 1 (len - 1) [@doesNotRaise]
      else
        (* "_rec" -> "rec" *)
        match Hashtbl.find table x with
        | y -> y
        | exception Not_found -> x
    else x
end

let mangleObjectField = Mangle.translate
