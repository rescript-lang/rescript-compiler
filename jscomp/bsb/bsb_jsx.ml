type version = Jsx_v3 | Jsx_v4
type module_ = React | Generic of {moduleName: string}
type mode = Classic | Automatic
type dependencies = string list

type t = {
  version : version option;
  module_ : module_ option;
  mode : mode option;
  v3_dependencies : dependencies;
}

let encode_no_nl jsx =
  (match jsx.version with
  | None -> ""
  | Some Jsx_v3 -> "3"
  | Some Jsx_v4 -> "4")
  ^ (match jsx.module_ with None -> "" | Some React -> "React" | Some Generic {moduleName} -> moduleName)
  ^
  match jsx.mode with
  | None -> ""
  | Some Classic -> "Classic"
  | Some Automatic -> "Automatic"

let ( .?() ) = Map_string.find_opt
let ( |? ) m (key, cb) = m |> Ext_json.test key cb

let get_list_string_acc (s : Ext_json_types.t array) acc =
  Ext_array.to_list_map_acc s acc (fun x ->
      match x with Str x -> Some x.str | _ -> None)

let get_list_string s = get_list_string_acc s []

let from_map map =
  let version : version option ref = ref None in
  let module_ : module_ option ref = ref None in
  let mode : mode option ref = ref None in
  let v3_dependencies : dependencies ref = ref [] in
  map
  |? ( Bsb_build_schemas.jsx,
       `Obj
         (fun m ->
           match m.?(Bsb_build_schemas.jsx_version) with
           | Some (Flo { loc; flo }) -> (
               match flo with
               | "3" -> version := Some Jsx_v3
               | "4" -> version := Some Jsx_v4
               | _ -> Bsb_exception.errorf ~loc "Unsupported jsx version %s" flo
               )
           | Some x ->
               Bsb_exception.config_error x
                 "Unexpected input (expect a version number) for jsx version"
           | None -> ()) )
  |? ( Bsb_build_schemas.jsx,
       `Obj
         (fun m ->
           match m.?(Bsb_build_schemas.jsx_module) with
           | Some (Str { str }) -> (
               match str with
               | "react" -> module_ := Some React
               | moduleName -> module_ := Some (Generic {moduleName}))
           | Some x ->
               Bsb_exception.config_error x
                 "Unexpected input (jsx module name) for jsx module"
           | None -> ()) )
  |? ( Bsb_build_schemas.jsx,
       `Obj
         (fun m ->
           match m.?(Bsb_build_schemas.jsx_mode) with
           | Some (Str { loc; str }) -> (
               match str with
               | "classic" -> mode := Some Classic
               | "automatic" -> mode := Some Automatic
               | _ -> Bsb_exception.errorf ~loc "Unsupported jsx mode %s" str)
           | Some x ->
               Bsb_exception.config_error x
                 "Unexpected input (expect classic or automatic) for jsx mode"
           | None -> ()) )
  |? ( Bsb_build_schemas.jsx,
       `Obj
         (fun m ->
           match m.?(Bsb_build_schemas.jsx_v3_dependencies) with
           | Some (Arr { content }) ->
            v3_dependencies := get_list_string content
           | Some x ->
               Bsb_exception.config_error x
                 "Unexpected input for jsx v3-dependencies"
           | None -> ()) )
  |> ignore;
  {
    version = !version;
    module_ = !module_;
    mode = !mode;
    v3_dependencies = !v3_dependencies;
  }
