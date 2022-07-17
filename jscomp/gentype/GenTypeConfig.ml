module ModuleNameMap = Map.Make (ModuleName)

type module_ = CommonJS | ES6
type bsVersion = int * int * int

type t = {
  mutable bsbProjectRoot : string;
  bsDependencies : string list;
  mutable emitImportCurry : bool;
  mutable emitImportReact : bool;
  mutable emitTypePropDone : bool;
  exportInterfaces : bool;
  generatedFileExtension : string option;
  module_ : module_;
  namespace : string option;
  platformLib : string;
  mutable projectRoot : string;
  shimsMap : ModuleName.t ModuleNameMap.t;
  sources : Ext_json_types.t option;
  suffix : string;
}

let default =
  {
    bsbProjectRoot = "";
    bsDependencies = [];
    emitImportCurry = false;
    emitImportReact = false;
    emitTypePropDone = false;
    exportInterfaces = false;
    generatedFileExtension = None;
    module_ = ES6;
    namespace = None;
    platformLib = "";
    projectRoot = "";
    shimsMap = ModuleNameMap.empty;
    sources = None;
    suffix = "";
  }

let bsPlatformLib ~config =
  match config.module_ with
  | ES6 -> config.platformLib ^ "/lib/es6"
  | CommonJS -> config.platformLib ^ "/lib/js"

let getBsCurryPath ~config = Filename.concat (bsPlatformLib ~config) "curry.js"

type map = Ext_json_types.t Map_string.t

let getOpt s (map : map) = Map_string.find_opt map s

let getBool s map =
  match map |> getOpt s with
  | Some (True _) -> Some true
  | Some (False _) -> Some false
  | _ -> None

let getStringOption s map =
  match map |> getOpt s with Some (Str { str }) -> Some str | _ -> None

let getShims map =
  let shims = ref [] in
  (match map |> getOpt "shims" with
  | Some (Obj { map = shimsMap }) ->
      Map_string.iter shimsMap (fun fromModule toModule ->
          match toModule with
          | Ext_json_types.Str { str } -> shims := (fromModule, str) :: !shims
          | _ -> ())
  | Some (Arr { content }) ->
      (* To be deprecated: array of strings *)
      content
      |> Array.iter (fun x ->
             match x with
             | Ext_json_types.Str { str } ->
                 let fromTo =
                   str |> String.split_on_char '=' |> Array.of_list
                 in
                 assert (Array.length fromTo == 2);
                 shims := (fromTo.(0), fromTo.(1)) :: !shims
             | _ -> ())
  | _ -> ());
  !shims

let setDebug ~gtconf =
  match gtconf |> getOpt "debug" with
  | Some (Obj { map }) -> Map_string.iter map Debug.setItem
  | _ -> ()

let compilerConfigFile = "bsconfig.json"

let rec findProjectRoot ~dir =
  if Sys.file_exists (Filename.concat dir compilerConfigFile) then dir
  else
    let parent = dir |> Filename.dirname in
    if parent = dir then (
      prerr_endline
        ("Error: cannot find project root containing " ^ compilerConfigFile
       ^ ".");
      assert false)
    else findProjectRoot ~dir:parent

let readConfig ~getBsConfigFile ~namespace =
  let projectRoot = findProjectRoot ~dir:(Sys.getcwd ()) in
  let bsbProjectRoot =
    match Sys.getenv_opt "BSB_PROJECT_ROOT" with
    | None -> projectRoot
    | Some s -> s
  in
  let parseConfig ~bsconf ~gtconf =
    let moduleString = gtconf |> getStringOption "module" in
    let exportInterfacesBool = gtconf |> getBool "exportInterfaces" in
    let generatedFileExtensionStringOption =
      gtconf |> getStringOption "generatedFileExtension"
    in
    let shimsMap =
      gtconf |> getShims
      |> List.fold_left
           (fun map (fromModule, toModule) ->
             let moduleName =
               (fromModule |> ModuleName.fromStringUnsafe : ModuleName.t)
             in
             let shimModuleName = toModule |> ModuleName.fromStringUnsafe in
             ModuleNameMap.add moduleName shimModuleName map)
           ModuleNameMap.empty
    in
    setDebug ~gtconf;
    let module_ =
      let packageSpecsModuleString =
        match bsconf |> getOpt "package-specs" with
        | Some (Obj { map = packageSpecs }) ->
            packageSpecs |> getStringOption "module"
        | _ -> None
      in
      (* Give priority to gentypeconfig, followed by package-specs *)
      match (moduleString, packageSpecsModuleString) with
      | Some "commonjs", _ -> CommonJS
      | Some "es6", _ -> ES6
      | None, Some "commonjs" -> CommonJS
      | None, Some ("es6" | "es6-global") -> ES6
      | _ -> default.module_
    in
    let exportInterfaces =
      match exportInterfacesBool with
      | None -> default.exportInterfaces
      | Some b -> b
    in
    let generatedFileExtension = generatedFileExtensionStringOption in
    let externalStdlib = bsconf |> getStringOption "external-stdlib" in
    let platformLib =
      match externalStdlib with
      | None -> "rescript"
      | Some externalStdlib -> externalStdlib
    in
    if !Debug.config then (
      Log_.item "Project root: %s\n" projectRoot;
      if bsbProjectRoot <> projectRoot then
        Log_.item "bsb project root: %s\n" bsbProjectRoot;
      Log_.item "Config module:%s shims:%d entries \n"
        (match moduleString with None -> "" | Some s -> s)
        (shimsMap |> ModuleNameMap.cardinal));
    let namespace =
      match bsconf |> getOpt "namespace" with
      | Some (True _) -> namespace
      | _ -> default.namespace
    in
    let suffix =
      match bsconf |> getStringOption "suffix" with
      | Some ".bs.js" -> ".bs"
      | Some s -> s
      | _ -> ".bs"
    in
    let bsDependencies =
      match bsconf |> getOpt "bs-dependencies" with
      | Some (Arr { content }) ->
          let strings = ref [] in
          content
          |> Array.iter (fun x ->
                 match x with
                 | Ext_json_types.Str { str } -> strings := str :: !strings
                 | _ -> ());
          !strings
      | _ -> default.bsDependencies
    in
    let sources =
      match bsconf |> getOpt "sources" with
      | Some sourceItem -> Some sourceItem
      | _ -> default.sources
    in
    {
      bsbProjectRoot;
      bsDependencies;
      suffix;
      emitImportCurry = false;
      emitImportReact = false;
      emitTypePropDone = false;
      exportInterfaces;
      generatedFileExtension;
      module_;
      namespace;
      platformLib;
      projectRoot;
      shimsMap;
      sources;
    }
  in
  match getBsConfigFile ~projectRoot with
  | Some bsConfigFile -> (
      try
        let json = bsConfigFile |> Ext_json_parse.parse_json_from_file in
        match json with
        | Obj { map = bsconf } -> (
            match bsconf |> getOpt "gentypeconfig" with
            | Some (Obj { map = gtconf }) -> parseConfig ~bsconf ~gtconf
            | _ -> default)
        | _ -> default
      with _ -> default)
  | None -> default
