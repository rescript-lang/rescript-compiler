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

let getOutputFileRelative ~config cmt =
  (cmt |> handleNamespace) ^ ModuleExtension.tsInputFileSuffix ~config

let getOutputFile ~(config : Config.t) cmt =
  Filename.concat config.projectRoot (getOutputFileRelative ~config cmt)

let getModuleName cmt =
  cmt |> handleNamespace |> Filename.basename |> ModuleName.fromStringUnsafe

let getCmtFile cmt =
  let pathCmt = Filename.concat (Sys.getcwd ()) cmt in
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
