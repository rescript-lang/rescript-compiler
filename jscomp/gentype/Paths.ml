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

let removePathPrefix ~prefix path =
  let normalizedPrefix = Filename.concat prefix "" in
  let prefixLen = String.length normalizedPrefix in
  let pathLen = String.length path in
  let isPrefix =
    prefixLen <= pathLen
    && (String.sub path 0 prefixLen [@doesNotRaise]) = normalizedPrefix
  in
  if isPrefix then
    String.sub path prefixLen (pathLen - prefixLen) [@doesNotRaise]
  else path

let appendSuffix ~config sourcePath =
  (sourcePath |> handleNamespace) ^ ModuleExtension.tsInputFileSuffix ~config

let getOutputFileRelative ~config sourcePath =
  if Filename.is_relative sourcePath then appendSuffix ~config sourcePath
  else
    let relative_path =
      removePathPrefix ~prefix:config.projectRoot sourcePath
    in
    appendSuffix ~config relative_path

let computeAbsoluteOutputFilePath ~(config : Config.t) path =
  Filename.concat config.projectRoot (getOutputFileRelative ~config path)

let getOutputFile ~(config : Config.t) sourcePath =
  if Filename.is_relative sourcePath then
    (* assuming a relative path from the project root *)
    computeAbsoluteOutputFilePath ~config sourcePath
  else
    (* for absolute paths we want to place the output beside the source file *)
    let relative_path =
      removePathPrefix ~prefix:config.projectRoot sourcePath
    in
    computeAbsoluteOutputFilePath ~config relative_path

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
