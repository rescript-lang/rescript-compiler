let ( |? ) m (key, cb) = m |> Ext_json.test key cb
let ( .?() ) = Map_string.find_opt

let get_list_string_acc (s : Ext_json_types.t array) acc =
  Ext_array.to_list_map_acc s acc (function
    | Str x -> Some x.str
    | _ -> None)

let get_list_string s = get_list_string_acc s []

type json_map = Ext_json_types.t Map_string.t

let extract_string (map : json_map) (field : string) =
  match map.?(field) with
  | None -> None
  | Some (Str { str }) -> Some(str)
  | Some manifest -> Bsb_exception.manifest_error manifest (field ^ " expects a string")

let extract_boolean (map : json_map) (field : string) (default : bool) : bool =
  match map.?(field) with
  | None -> default
  | Some (True _) -> true
  | Some (False _) -> false
  | Some manifest ->
      Bsb_exception.manifest_error manifest (field ^ " expects a boolean")

(* return an empty array if not found *)
let extract_string_list (map : json_map) (field : string) =

  match map.?(field) with
  | None -> []
  | Some (Arr { content = s }) -> get_list_string s
  | Some manifest -> Bsb_exception.manifest_error manifest (field ^ " expects an array of string")

let extract_package_name_and_namespace (map : json_map) : string * string option
    =
  let package_name =
    match map.?(Bsb_build_schemas.name) with
    | Some (Str { str = "_" } as manifest) ->
        Bsb_exception.manifest_error manifest "_ is a reserved package name"
    | Some (Str { str = name }) -> name
    | Some manifest ->
        Bsb_exception.manifest_error manifest "name expect a string field"
    | None -> Bsb_exception.invalid_spec "field name is required"
  in
  let namespace =
    match map.?(Bsb_build_schemas.namespace) with
    | None | Some (False _) -> None
    | Some (True _) ->
        Some (Ext_namespace.namespace_of_package_name package_name)
    | Some (Str { str }) ->
        (*TODO : check the validity of namespace *)
        Some (Ext_namespace.namespace_of_package_name str)
    | Some manifest ->
        Bsb_exception.manifest_error manifest "namespace field expects string or boolean"
  in
  (package_name, namespace)

let extract_reason_react (map : json_map) =
  let open Bsb_manifest_types.ReasonReact in
  let default : t option ref = ref None in
  map
  |? ( Bsb_build_schemas.reason,
       `Obj
         (fun m ->
           match m.?(Bsb_build_schemas.react_jsx) with
           | Some (Flo { loc; flo }) -> (
               match flo with
               | "3" -> default := Some { react_jsx = Jsx_v3 }
               | _ -> Bsb_exception.errorf ~loc "Unsupported jsx version %s" flo
               )
           | Some x ->
               Bsb_exception.manifest_error x
                 "Unexpected input (expect a version number) for jsx, note \
                  boolean is no longer allowed"
           | None -> ()) )
  |> ignore;
  !default

let extract_warning (map : json_map) =
  let open Bsb_manifest_types.Warning in
  match map.?(Bsb_build_schemas.warnings) with
  | None -> None
  | Some (Obj { map }) -> (
    let number_opt = Map_string.find_opt map Bsb_build_schemas.number in
    let error_opt = Map_string.find_opt map Bsb_build_schemas.error in
    match (number_opt, error_opt) with
    | None, None -> None
    | _, _ ->
        let error =
          match error_opt with
          | Some (True _) -> Warn_error_true
          | Some (False _) -> Warn_error_false
          | Some (Str { str }) -> Warn_error_number str
          | Some x -> Bsb_exception.manifest_error x "expect boolean or string"
          | None -> Warn_error_false
          (* To make it less intrusive : warning error has to be enabled*)
        in
        let number =
          match number_opt with
          | Some (Str { str = number }) -> Some number
          | None -> None
          | Some x -> Bsb_exception.manifest_error x "expect a string"
        in
        Some { number; error }
  )
  | Some config -> Bsb_exception.manifest_error config "expect an object"

let extract_generators (map : json_map) =
  let generators = ref Map_string.empty in
  (match map.?(Bsb_build_schemas.generators) with
  | None -> ()
  | Some (Arr { content = s }) ->
      generators :=
        Ext_array.fold_left s Map_string.empty (fun acc json ->
            match json with
            | Obj { map = m; loc } -> (
                match
                  (m.?(Bsb_build_schemas.name), m.?(Bsb_build_schemas.command))
                with
                | Some (Str { str = name }), Some (Str { str = command }) ->
                    Map_string.add acc name command
                | _, _ ->
                    Bsb_exception.errorf ~loc
                      {| generators exepect format like { "name" : "cppo",  "command"  : "cppo $in -o $out"} |}
                )
            | _ -> acc)
  | Some manifest ->
      Bsb_exception.manifest_error manifest
        (Bsb_build_schemas.generators ^ " expect an array field"));
  !generators

let extract_package_specs (map : json_map) ~suffix =
  let supported_format (x : string) loc : Ext_module_system.t =
    if x = Literals.commonjs then NodeJS
    else if x = Literals.es6 then Es6
    else if x = Literals.es6_global then Es6_global
    else
      Bsb_exception.errorf ~loc
      "package-specs: `%s` isn't a valid output module format. It has to be one \
      of:  %s, %s or %s"
      x Literals.commonjs Literals.es6 Literals.es6_global
  in
  let from_json_single suffix (x : Ext_json_types.t) : Bsb_manifest_types.package_spec =
    match x with
    | Str { str = format; loc } ->
        { format = supported_format format loc; in_source = false; suffix }
    | Obj { map; loc } -> (
        match map.?("module") with
        | Some (Str { str = format }) ->
            let in_source =
              match map.?(Bsb_build_schemas.in_source) with
              | Some (True _) -> true
              | Some _ | None -> false
            in
            let suffix =
              match map.?(Bsb_build_schemas.suffix) with
              | Some (Str { str = suffix; loc }) ->
                  let s = Ext_js_suffix.of_string suffix in
                  if s = Unknown_extension then
                    Bsb_exception.errorf ~loc "expect .js,.bs.js,.mjs or .cjs"
                  else s
              | Some _ ->
                  Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
                    "expect a string field"
              | None -> suffix
            in
            { format = supported_format format loc; in_source; suffix }
        | Some _ ->
            Bsb_exception.errorf ~loc
              "package-specs: when the configuration is an object, `module` \
              field should be a string, not an array. If you want to pass \
              multiple module specs, try turning package-specs into an array of \
              objects (or strings) instead."
        | None ->
            Bsb_exception.errorf ~loc
              "package-specs: when the configuration is an object, the `module` \
              field is mandatory.")
    | _ ->
        Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
          "package-specs: we expect either a string or an object."
  in
  let from_json_array suffix arr =
    let spec = ref Bsb_spec_set.empty in
    let has_in_source = ref false in
    Ext_array.iter arr (fun x ->
        let result = from_json_single suffix x in
        if result.in_source then
          if not !has_in_source then has_in_source := true
          else
            Bsb_exception.errorf ~loc:(Ext_json.loc_of x)
              "package-specs: we've detected two module formats that are both \
              configured to be in-source.";
        spec := Bsb_spec_set.add result !spec);
    !spec
    (* TODO: FIXME: better API without mutating *)
  in
  match map.?(Bsb_build_schemas.package_specs) with
  | Some (Arr { content }) -> from_json_array suffix content
  | Some _ | None -> Bsb_spec_set.singleton ({ format = NodeJS; in_source = false; suffix })

let extract_ppx_specs (map : json_map) =
  let field = Bsb_build_schemas.ppx_flags in
  match map.?(field) with
  | None -> []
  | Some (Arr { content }) ->
      Ext_array.to_list_f content (fun x ->
          match x with
          | Str x -> { Bsb_manifest_types.name = x.str; args = [] }
          | Arr { content } -> (
              let xs = get_list_string content in
              match xs with
              | [] -> Bsb_exception.manifest_error x " empty array is not allowed"
              | name :: args -> { Bsb_manifest_types.name = name; args })
          | x ->
              Bsb_exception.manifest_error x
                (field ^ "expect each item to be either string or array"))
  | Some manifest -> Bsb_exception.manifest_error manifest (field ^ " expect an array")

let extract_suffix (map : json_map) : Ext_js_suffix.t =
  match map.?(Bsb_build_schemas.suffix) with
  | None -> Js
  | Some (Str { str; loc }) ->
      let s = Ext_js_suffix.of_string str in
      if s = Unknown_extension then
        Bsb_exception.errorf ~loc
          "expect .js, .mjs, .cjs or .bs.js, .bs.mjs, .bs.cjs here"
      else s
  | Some manifest ->
      Bsb_exception.manifest_error manifest
        "expect a string exteion like \".js\" here"

let extract_js_post_build (map : json_map) =
  let js_post_build_cmd = ref None in
  map
  |? ( Bsb_build_schemas.js_post_build,
       `Obj
         (fun m ->
           m
           |? ( Bsb_build_schemas.cmd,
                `Str (fun s -> js_post_build_cmd := Some(s)) )
           |> ignore) )
  |> ignore;
  !js_post_build_cmd

let extract_jsx (map : json_map) =
  let open Bsb_manifest_types.Jsx in
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
               Bsb_exception.manifest_error x
                 "Unexpected input (expect a version number) for jsx version"
           | None -> ()) )
  |? ( Bsb_build_schemas.jsx,
       `Obj
         (fun m ->
           match m.?(Bsb_build_schemas.jsx_module) with
           | Some (Str { loc; str }) -> (
               match str with
               | "react" -> module_ := Some React
               | _ -> Bsb_exception.errorf ~loc "Unsupported jsx module %s" str)
           | Some x ->
               Bsb_exception.manifest_error x
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
               Bsb_exception.manifest_error x
                 "Unexpected input (expect classic or automatic) for jsx mode"
           | None -> ()) )
  |? ( Bsb_build_schemas.jsx,
       `Obj
         (fun m ->
           match m.?(Bsb_build_schemas.jsx_v3_dependencies) with
           | Some (Arr { content }) ->
            v3_dependencies := get_list_string content
           | Some x ->
               Bsb_exception.manifest_error x
                 "Unexpected input for jsx v3-dependencies"
           | None -> ()) )
  |> ignore;
  {
    version = !version;
    module_ = !module_;
    mode = !mode;
    v3_dependencies = !v3_dependencies;
  }

let extract_gentype (map : json_map) ~package_specs =
  let open Bsb_manifest_types.Gentype in
  match map.?(Bsb_build_schemas.gentype) with
  | None -> None
  | Some (Obj { map }) -> (
    let { Bsb_manifest_types.format } = List.hd package_specs in
    let module_ =
      match (extract_string map Bsb_build_schemas.gentype_module, format) with
      | Some "commonjs", _ -> CommonJS
      | Some "es6", _ -> ES6
      | None, NodeJS -> CommonJS
      | None, (Es6 | Es6_global) -> ES6
      | _ -> ES6
    in
    let moduleResolution =
      match extract_string map Bsb_build_schemas.gentype_module_resolution with
      | Some "node" -> Node
      | Some "node16" -> Node16
      | Some "bundler" -> Bundler
      | _ -> Node
    in
    let shims =
      match map.?(Bsb_build_schemas.gentype_shims) with
      | None -> Map_string.empty
      | Some (Obj { map }) ->
        Map_string.map map
          (fun x ->
            match x with
            | Str { str } -> str
            | x -> Bsb_exception.manifest_error x "shims expects a record"
        )
      | Some manifest ->
          Bsb_exception.manifest_error manifest "shims expects a record"
    in
    let debug =
      match map.?(Bsb_build_schemas.gentype_debug) with
      | Some (Obj { map }) -> Some map
      | _ -> None
    in
    Some {
      module_;
      moduleResolution;
      shims;
      debug;
      exportInterfaces = extract_boolean map Bsb_build_schemas.gentype_export_interfaces false;
      generatedFileExtension = extract_string map Bsb_build_schemas.gentype_generated_file_extension;
    }
  )
  | Some manifest -> Bsb_exception.manifest_error manifest "expect an object"
