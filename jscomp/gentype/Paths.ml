open GenTypeCommon

let concat = Filename.concat

let handleNamespace cmt =
  let cutAfterDash s =
    match String.index s '-' with
    | n -> String.sub s 0 n [@doesNotRaise]
    | exception Not_found -> s
  in
  let noDir = Filename.basename cmt = cmt in
  if noDir then cmt |> (Filename.chop_extension [@doesNotRaise]) |> cutAfterDash
  else
    let dir = cmt |> Filename.dirname in
    let base =
      cmt |> Filename.basename |> (Filename.chop_extension [@doesNotRaise])
      |> cutAfterDash
    in
    Filename.concat dir base

let findNameSpace cmt =
  let keepAfterDash s =
    match String.index s '-' with
    | n ->
      Some ((String.sub s (n + 1) [@doesNotRaise]) (String.length s - n - 1))
    | exception Not_found -> None
  in
  cmt |> Filename.basename |> (Filename.chop_extension [@doesNotRaise])
  |> keepAfterDash

let remove_project_root_from_absolute_path ~(config : Config.t) source_path =
  let i = String.length config.projectRoot + 1 in
  let n = String.length source_path - i in
  (String.sub source_path i n [@doesNotRaise])

let getOutputFileRelative ~config source_path =
  if Filename.is_relative source_path then
    (source_path |> handleNamespace) ^ ModuleExtension.tsInputFileSuffix ~config
  else
    let relative_path =
      remove_project_root_from_absolute_path ~config source_path
    in
    (relative_path |> handleNamespace)
    ^ ModuleExtension.tsInputFileSuffix ~config

let getOutputFile ~(config : Config.t) sourcePath =
  if Filename.is_relative sourcePath then
    (* assuming a relative path from the project root *)
    Filename.concat config.projectRoot
      (getOutputFileRelative ~config sourcePath)
  else
    (* we want to place the output beside the source file *)
    let relative_path =
      remove_project_root_from_absolute_path ~config sourcePath
    in
    Filename.concat config.projectRoot
      (getOutputFileRelative ~config relative_path)

let getModuleName cmt =
  cmt |> handleNamespace |> Filename.basename |> ModuleName.fromStringUnsafe

let getCmtFile cmt =
  let pathCmt =
    if Filename.is_relative cmt then Filename.concat (Sys.getcwd ()) cmt
    else cmt
  in
  let cmtFile =
    if Filename.check_suffix pathCmt ".cmt" then
      let pathCmtLowerCase =
        let dirName = pathCmt |> Filename.dirname in
        let baseName = pathCmt |> Filename.basename in
        Filename.concat dirName (baseName |> String.uncapitalize_ascii)
      in
      let pathCmti =
        (Filename.chop_extension pathCmt [@doesNotRaise]) ^ ".cmti"
      in
      let pathCmtiLowerCase =
        (Filename.chop_extension pathCmtLowerCase [@doesNotRaise]) ^ ".cmti"
      in
      if Sys.file_exists pathCmtiLowerCase then pathCmtiLowerCase
      else if Sys.file_exists pathCmti then pathCmti
      else if Sys.file_exists pathCmtLowerCase then pathCmtLowerCase
      else if Sys.file_exists pathCmt then pathCmt
      else ""
    else ""
  in
  cmtFile

let getConfigFile ~projectRoot =
  let config = concat projectRoot Config.compilerConfigFile in
  match config |> Sys.file_exists with
  | true -> Some config
  | false -> (
    let config = concat projectRoot Config.legacyCompilerConfigFile in
    match config |> Sys.file_exists with
    | true -> Some config
    | false -> None)

let readConfig ~namespace = Config.readConfig ~getConfigFile ~namespace
