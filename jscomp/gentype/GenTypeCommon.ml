module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module Config = GenTypeConfig

module DocString = struct
  type t = string option
  let render t =
    match t with
    | None | Some "" -> ""
    | Some docString -> "/** " ^ String.trim docString ^ " */\n"
  let empty = None
  let hasContent docString = Option.is_some docString
end

let logNotImplemented x =
  if !Debug.notImplemented then Log_.item "Not Implemented: %s\n" x

type optional = Mandatory | Optional
type mutable_ = Immutable | Mutable

type labelJS =
  | NullLabel
  | UndefinedLabel
  | BoolLabel of bool
  | FloatLabel of string
  | IntLabel of string
  | StringLabel of string

type case = {labelJS: labelJS}

let isJSSafePropertyName name =
  name = ""
  || (match name.[0] [@doesNotRaise] with
     | 'A' .. 'z' -> true
     | _ -> false)
     && name
        |> String.for_all (function
             | 'A' .. 'z' | '0' .. '9' -> true
             | _ -> false)

let isNumber s =
  let len = String.length s in
  len > 0
  && (match len > 1 with
     | true -> (s.[0] [@doesNotRaise]) > '0'
     | false -> true)
  &&
  let res = ref true in
  for i = 0 to len - 1 do
    match s.[i] [@doesNotRaise] with
    | '0' .. '9' -> ()
    | _ -> res := false
  done;
  res.contents

let labelJSToString case =
  match case.labelJS with
  | NullLabel -> "null"
  | UndefinedLabel -> "undefined"
  | BoolLabel b -> b |> string_of_bool
  | FloatLabel s -> s
  | IntLabel i -> i
  | StringLabel s -> s |> EmitText.quotes

type closedFlag = Open | Closed | Inline

type type_ =
  | Array of type_ * mutable_
  | Dict of type_
  | Function of function_
  | Ident of ident
  | Null of type_
  | Nullable of type_
  | Object of closedFlag * fields
  | Option of type_
  | Promise of type_
  | Tuple of type_ list
  | TypeVar of string
  | Variant of variant (* ordinary and polymorphic variants *)

and fields = field list
and argType = {aName: string; aType: type_}

and field = {
  mutable_: mutable_;
  nameJS: string;
  optional: optional;
  type_: type_;
  docString: DocString.t;
}

and function_ = {argTypes: argType list; retType: type_; typeVars: string list}

and ident = {builtin: bool; name: string; typeArgs: type_ list}

and variant = {
  inherits: type_ list;
  noPayloads: case list;
  payloads: payload list;
  polymorphic: bool; (* If true, this is a polymorphic variant *)
  tag: string option; (* The name of the tag field at runtime *)
  unboxed: bool;
}

and payload = {case: case; t: type_}

type label = Nolabel | Label of string | OptLabel of string

type dep =
  | External of string
  | Internal of ResolvedName.t
  | Dot of dep * string

module ScopedPackage = (* Taken from ext_namespace.ml in bukclescript *)
struct
  let namespace_of_package_name (s : string) : string =
    let len = String.length s in
    let buf = Buffer.create len in
    let add capital ch =
      Buffer.add_char buf (if capital then Char.uppercase_ascii ch else ch)
    in
    let rec aux capital off len =
      if off >= len then ()
      else
        let ch = String.unsafe_get s off in
        match ch with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
          add capital ch;
          aux false (off + 1) len
        | '/' | '-' -> aux true (off + 1) len
        | _ -> aux capital (off + 1) len
    in
    aux true 0 len;
    Buffer.contents buf

  (** @demo/some-library -> DemoSomelibrary *)
  let packageNameToGeneratedModuleName packageName =
    if String.contains packageName '/' then
      Some (packageName |> namespace_of_package_name)
    else None

  let isGeneratedModule id ~(config : Config.t) =
    config.bsDependencies
    |> List.exists (fun packageName ->
           packageName |> packageNameToGeneratedModuleName
           = Some (id |> Ident.name))

  (** (Common, DemoSomelibrary) -> Common-DemoSomelibrary *)
  let addGeneratedModule s ~generatedModule =
    s ^ "-" ^ Ident.name generatedModule

  (** Common-DemoSomelibrary -> Common *)
  let removeGeneratedModule s =
    match s |> String.split_on_char '-' with
    | [name; _scope] -> name
    | _ -> s
end

let rec depToString dep =
  match dep with
  | External name -> name |> ScopedPackage.removeGeneratedModule
  | Internal resolvedName -> resolvedName |> ResolvedName.toString
  | Dot (d, s) -> depToString d ^ "_" ^ s

let rec depToResolvedName (dep : dep) =
  match dep with
  | External name -> name |> ResolvedName.fromString
  | Internal resolvedName -> resolvedName
  | Dot (p, s) -> ResolvedName.dot s (p |> depToResolvedName)

let createVariant ~inherits ~noPayloads ~payloads ~polymorphic ~tag ~unboxed =
  Variant {inherits; noPayloads; payloads; polymorphic; tag; unboxed}

let ident ?(builtin = true) ?(typeArgs = []) name =
  Ident {builtin; name; typeArgs}

let sanitizeTypeName name =
  name
  |> String.map (function
       | '\'' -> '_'
       | c -> c)
let unknown = ident "unknown"
let bigintT = ident "BigInt"
let booleanT = ident "boolean"
let dateT = ident "Date"
let mapT (x, y) = ident ~typeArgs:[x; y] "Map"
let numberT = ident "number"
let regexpT = ident "RegExp"
let setT x = ident ~typeArgs:[x] "Set"
let stringT = ident "string"
let unitT = ident "void"
let weakmapT (x, y) = ident ~typeArgs:[x; y] "WeakMap"
let weaksetT x = ident ~typeArgs:[x] "WeakSet"
let int64T = Tuple [numberT; numberT]

module NodeFilename = struct
  include Filename

  (* Force "/" separator. *)
  let dirSep = "/"

  module Path : sig
    type t

    val normalize : string -> t
    val concat : t -> string -> t
    val toString : t -> string
  end = struct
    type t = string

    let normalize path : t =
      match Sys.os_type with
      | "Win32" -> path |> String.split_on_char '\\' |> String.concat dirSep
      | _ -> path

    let toString path = path
    let length path = String.length path

    let concat dirname filename =
      let isDirSep s i =
        let c = (s.[i] [@doesNotRaise]) in
        c = '/' || c = '\\' || c = ':'
      in
      let l = length dirname in
      if l = 0 || isDirSep dirname (l - 1) then dirname ^ filename
      else dirname ^ dirSep ^ filename
  end

  let concat (dirname : string) filename =
    let open Path in
    Path.concat (normalize dirname) filename |> toString
end
