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

let jsVariantTag ~polymorphic ~tag =
  match polymorphic with
  | true -> "NAME"
  | false -> (
    match tag with
    | Some tag -> tag
    | None -> "TAG")

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
