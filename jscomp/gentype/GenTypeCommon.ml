module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module Config = GenTypeConfig

let logNotImplemented x =
  if !Debug.notImplemented then Log_.item "Not Implemented: %s\n" x

type optional = Mandatory | Optional
type mutable_ = Immutable | Mutable

type labelJS =
  | BoolLabel of bool
  | FloatLabel of string
  | IntLabel of string
  | StringLabel of string

type case = { label : string; labelJS : labelJS }

let isJSSafePropertyName name =
  name = ""
  || (match name.[0] with 'A' .. 'z' -> true | _ -> false)
     && name
        |> String.for_all (function
             | 'A' .. 'z' | '0' .. '9' -> true
             | _ -> false)

let labelJSToString ?(alwaysQuotes = false) case =
  let addQuotes x =
    match alwaysQuotes with true -> x |> EmitText.quotes | false -> x
  in
  let isNumber s =
    let len = String.length s in
    len > 0
    && (match len > 1 with true -> s.[0] > '0' | false -> true)
    &&
    let res = ref true in
    for i = 0 to len - 1 do
      match s.[i] with '0' .. '9' -> () | _ -> res := false
    done;
    res.contents
  in
  match case.labelJS with
  | BoolLabel b -> b |> string_of_bool |> addQuotes
  | FloatLabel s -> s |> addQuotes
  | IntLabel i -> i |> addQuotes
  | StringLabel s ->
      if s = case.label && isNumber s then s |> addQuotes
      else s |> EmitText.quotes

type closedFlag = Open | Closed

type type_ =
  | Array of type_ * mutable_
  | Function of function_
  | GroupOfLabeledArgs of fields
  | Ident of ident
  | Null of type_
  | Nullable of type_
  | Object of closedFlag * fields
  | Option of type_
  | Promise of type_
  | Record of fields
  | Tuple of type_ list
  | TypeVar of string
  | Variant of variant

and fields = field list
and argType = { aName : string; aType : type_ }

and field = {
  mutable_ : mutable_;
  nameJS : string;
  nameRE : string;
  optional : optional;
  type_ : type_;
}

and function_ = {
  argTypes : argType list;
  componentName : string option;
  retType : type_;
  typeVars : string list;
  uncurried : bool;
}

and ident = { builtin : bool; name : string; typeArgs : type_ list }

and variant = {
  bsStringOrInt : bool;
  hash : int;
  inherits : type_ list;
  noPayloads : case list;
  payloads : payload list;
  polymorphic : bool;
  unboxed : bool;
}

and payload = { case : case; inlineRecord : bool; numArgs : int; t : type_ }

let typeIsObject type_ =
  match type_ with
  | Array _ -> true
  | Function _ -> false
  | GroupOfLabeledArgs _ -> false
  | Ident _ -> false
  | Null _ -> false
  | Nullable _ -> false
  | Object _ -> true
  | Option _ -> false
  | Promise _ -> true
  | Record _ -> true
  | Tuple _ -> true
  | TypeVar _ -> false
  | Variant _ -> false

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
    match s |> String.split_on_char '-' with [ name; _scope ] -> name | _ -> s
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

let createVariant ~bsStringOrInt ~inherits ~noPayloads ~payloads ~polymorphic =
  let hash =
    noPayloads
    |> List.map (fun case -> (case.label, case.labelJS))
    |> Array.of_list |> Hashtbl.hash
  in
  let unboxed = payloads = [] in
  Variant
    {
      bsStringOrInt;
      hash;
      inherits;
      noPayloads;
      payloads;
      polymorphic;
      unboxed;
    }

let variantTable hash ~toJS =
  (match toJS with true -> "$$toJS" | false -> "$$toRE") ^ string_of_int hash

let ident ?(builtin = true) ?(typeArgs = []) name =
  Ident { builtin; name; typeArgs }

let sanitizeTypeName name = name |> String.map (function '\'' -> '_' | c -> c)
let unknown = ident "unknown"
let booleanT = ident "boolean"
let dateT = ident "Date"
let numberT = ident "number"
let stringT = ident "string"
let unitT = ident "void"
let int64T = Tuple [ numberT; numberT ]

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
        let c = s.[i] in
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
