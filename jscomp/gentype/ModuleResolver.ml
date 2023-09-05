open GenTypeCommon
module ModuleNameMap = Map.Make (ModuleName)

let ( +++ ) = Filename.concat

(** Read all the dirs from a library in node_modules *)
let readBsDependenciesDirs ~root =
  let dirs = ref [] in
  let rec findSubDirs dir =
    let absDir =
      match dir = "" with
      | true -> root
      | false -> root +++ dir
    in
    if Sys.file_exists absDir && Sys.is_directory absDir then (
      dirs := dir :: !dirs;
      absDir |> Sys.readdir |> Array.iter (fun d -> findSubDirs (dir +++ d)))
  in
  findSubDirs "";
  !dirs

type pkgs = {dirs: string list; pkgs: (string, string) Hashtbl.t}

let readDirsFromConfig ~(config : Config.t) =
  let dirs = ref [] in
  let root = config.projectRoot in
  let ( +++ ) = Filename.concat in
  let rec processDir ~subdirs dir =
    let absDir =
      match dir = "" with
      | true -> root
      | false -> root +++ dir
    in
    if Sys.file_exists absDir && Sys.is_directory absDir then (
      dirs := dir :: !dirs;
      if subdirs then
        absDir |> Sys.readdir
        |> Array.iter (fun d -> processDir ~subdirs (dir +++ d)))
  in
  let rec processSourceItem (sourceItem : Ext_json_types.t) =
    match sourceItem with
    | Str {str} -> str |> processDir ~subdirs:false
    | Obj {map} -> (
      match Map_string.find_opt map "dir" with
      | Some (Str {str}) ->
        let subdirs =
          match Map_string.find_opt map "subdirs" with
          | Some (True _) -> true
          | Some (False _) -> false
          | _ -> false
        in
        str |> processDir ~subdirs
      | _ -> ())
    | Arr {content} -> Array.iter processSourceItem content
    | _ -> ()
  in
  (match config.sources with
  | Some sourceItem -> processSourceItem sourceItem
  | None -> ());
  !dirs

let readSourceDirs ~(config : Config.t) =
  let sourceDirs =
    ["lib"; "bs"; ".sourcedirs.json"]
    |> List.fold_left ( +++ ) config.bsbProjectRoot
  in
  let dirs = ref [] in
  let pkgs = Hashtbl.create 1 in
  let readDirs json =
    match json with
    | Ext_json_types.Obj {map} -> (
      match Map_string.find_opt map "dirs" with
      | Some (Arr {content}) ->
        content
        |> Array.iter (fun x ->
               match x with
               | Ext_json_types.Str {str} -> dirs := str :: !dirs
               | _ -> ());
        ()
      | _ -> ())
    | _ -> ()
  in
  let readPkgs json =
    match json with
    | Ext_json_types.Obj {map} -> (
      match Map_string.find_opt map "pkgs" with
      | Some (Arr {content}) ->
        content
        |> Array.iter (fun x ->
               match x with
               | Ext_json_types.Arr
                   {content = [|Str {str = name}; Str {str = path}|]} ->
                 Hashtbl.add pkgs name path
               | _ -> ());
        ()
      | _ -> ())
    | _ -> ()
  in
  if sourceDirs |> Sys.file_exists then
    try
      let json = sourceDirs |> Ext_json_parse.parse_json_from_file in
      if config.bsbProjectRoot <> config.projectRoot then
        dirs := readDirsFromConfig ~config
      else readDirs json;
      readPkgs json
    with _ -> ()
  else (
    Log_.item "Warning: can't find source dirs: %s\n" sourceDirs;
    Log_.item "Types for cross-references will not be found by genType.\n";
    dirs := readDirsFromConfig ~config);
  {dirs = !dirs; pkgs}

(** Read the project's .sourcedirs.json file if it exists
   and build a map of the files with the given extension
   back to the directory where they belong. *)
let sourcedirsJsonToMap ~config ~extensions ~excludeFile =
  let rec chopExtensions fname =
    match fname |> Filename.chop_extension with
    | fnameChopped -> fnameChopped |> chopExtensions
    | exception _ -> fname
  in
  let fileMap = ref ModuleNameMap.empty in
  let bsDependenciesFileMap = ref ModuleNameMap.empty in
  let filterGivenExtension fileName =
    extensions |> List.exists (fun ext -> Filename.check_suffix fileName ext)
    && not (excludeFile fileName)
  in
  let addDir ~dirOnDisk ~dirEmitted ~filter ~map =
    dirOnDisk |> Sys.readdir
    |> Array.iter (fun fname ->
           if fname |> filter then
             map :=
               !map
               |> ModuleNameMap.add
                    (fname |> chopExtensions |> ModuleName.fromStringUnsafe)
                    dirEmitted)
  in
  let {dirs; pkgs} = readSourceDirs ~config in
  dirs
  |> List.iter (fun dir ->
         addDir ~dirEmitted:dir
           ~dirOnDisk:(config.projectRoot +++ dir)
           ~filter:filterGivenExtension ~map:fileMap);
  config.bsDependencies
  |> List.iter (fun packageName ->
         match Hashtbl.find pkgs packageName with
         | path ->
           let root = ["lib"; "bs"] |> List.fold_left ( +++ ) path in
           let filter fileName =
             [".cmt"; ".cmti"]
             |> List.exists (fun ext -> Filename.check_suffix fileName ext)
           in
           readBsDependenciesDirs ~root
           |> List.iter (fun dir ->
                  let dirOnDisk = root +++ dir in
                  let dirEmitted = packageName +++ dir in
                  addDir ~dirEmitted ~dirOnDisk ~filter
                    ~map:bsDependenciesFileMap)
         | exception Not_found -> ());
  (!fileMap, !bsDependenciesFileMap)

type case = Lowercase | Uppercase

type resolver = {
  lazyFind:
    (useBsDependencies:bool -> ModuleName.t -> (string * case * bool) option)
    Lazy.t;
}

let createLazyResolver ~config ~extensions ~excludeFile =
  {
    lazyFind =
      lazy
        (let moduleNameMap, bsDependenciesFileMap =
           sourcedirsJsonToMap ~config ~extensions ~excludeFile
         in
         let find ~bsDependencies ~map moduleName =
           match map |> ModuleNameMap.find moduleName with
           | resolvedModuleDir ->
             Some (resolvedModuleDir, Uppercase, bsDependencies)
           | exception Not_found -> (
             match
               map |> ModuleNameMap.find (moduleName |> ModuleName.uncapitalize)
             with
             | resolvedModuleDir ->
               Some (resolvedModuleDir, Lowercase, bsDependencies)
             | exception Not_found -> None)
         in
         fun ~useBsDependencies moduleName ->
           match
             moduleName |> find ~bsDependencies:false ~map:moduleNameMap
           with
           | None when useBsDependencies ->
             moduleName |> find ~bsDependencies:true ~map:bsDependenciesFileMap
           | res -> res);
  }

let apply ~resolver ~useBsDependencies moduleName =
  moduleName |> Lazy.force resolver.lazyFind ~useBsDependencies

(** Resolve a reference to ModuleName, and produce a path suitable for require.
   E.g. require "../foo/bar/ModuleName.ext" where ext is ".res" or ".js". *)
let resolveModule ~(config : Config.t) ~importExtension ~outputFileRelative
    ~resolver ~useBsDependencies moduleName =
  let outputFileRelativeDir =
    (* e.g. src if we're generating src/File.bs.js *)
    Filename.dirname outputFileRelative
  in
  let outputFileAbsoluteDir = config.projectRoot +++ outputFileRelativeDir in
  let moduleNameResFile =
    (* Check if the module is in the same directory as the file being generated.
       So if e.g. project_root/src/ModuleName.res exists. *)
    outputFileAbsoluteDir +++ (ModuleName.toString moduleName ^ ".res")
  in
  let candidate =
    (* e.g. import "./Modulename.ext" *)
    moduleName
    |> ImportPath.fromModule ~dir:Filename.current_dir_name ~importExtension
  in
  if Sys.file_exists moduleNameResFile then candidate
  else
    let rec pathToList path =
      let isRoot = path |> Filename.basename = path in
      match isRoot with
      | true -> [path]
      | false ->
        (path |> Filename.basename) :: (path |> Filename.dirname |> pathToList)
    in
    match moduleName |> apply ~resolver ~useBsDependencies with
    | None -> candidate
    | Some (resolvedModuleDir, case, bsDependencies) ->
      (* e.g. "dst" in case of dst/ModuleName.res *)
      let walkUpOutputDir =
        outputFileRelativeDir |> pathToList
        |> List.map (fun _ -> Filename.parent_dir_name)
        |> fun l ->
        match l with
        | [] -> ""
        | _ :: rest -> rest |> List.fold_left ( +++ ) Filename.parent_dir_name
      in
      let fromOutputDirToModuleDir =
        (* e.g. "../dst" *)
        match bsDependencies with
        | true -> resolvedModuleDir
        | false -> walkUpOutputDir +++ resolvedModuleDir
      in
      (* e.g. import "../dst/ModuleName.ext" *)
      (match case = Uppercase with
      | true -> moduleName
      | false -> moduleName |> ModuleName.uncapitalize)
      |> ImportPath.fromModule ~dir:fromOutputDirToModuleDir ~importExtension

let resolveGeneratedModule ~config ~outputFileRelative ~resolver moduleName =
  if !Debug.moduleResolution then
    Log_.item "Resolve Generated Module: %s\n"
      (moduleName |> ModuleName.toString);
  let importPath =
    resolveModule ~config
      ~importExtension:(ModuleExtension.generatedModuleExtension ~config)
      ~outputFileRelative ~resolver ~useBsDependencies:true moduleName
  in
  if !Debug.moduleResolution then
    Log_.item "Import Path: %s\n" (importPath |> ImportPath.dump);
  importPath

(** Returns the path to import a given Reason module name. *)
let importPathForReasonModuleName ~(config : Config.t) ~outputFileRelative
    ~resolver moduleName =
  if !Debug.moduleResolution then
    Log_.item "Resolve Reason Module: %s\n" (moduleName |> ModuleName.toString);
  match config.shimsMap |> ModuleNameMap.find moduleName with
  | shimModuleName ->
    if !Debug.moduleResolution then
      Log_.item "ShimModuleName: %s\n" (shimModuleName |> ModuleName.toString);
    let importExtension = ModuleExtension.shimTsOutputFileExtension ~config in
    let importPath =
      resolveModule ~config ~importExtension ~outputFileRelative ~resolver
        ~useBsDependencies:false shimModuleName
    in
    if !Debug.moduleResolution then
      Log_.item "Import Path: %s\n" (importPath |> ImportPath.dump);
    importPath
  | exception Not_found ->
    moduleName |> resolveGeneratedModule ~config ~outputFileRelative ~resolver
