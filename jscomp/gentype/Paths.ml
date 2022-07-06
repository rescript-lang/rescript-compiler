open GenTypeCommon

let bsconfig = "bsconfig.json"

let rec findProjectRoot ~dir =
  if Sys.file_exists (Filename.concat dir bsconfig) then dir
  else
    let parent = dir |> Filename.dirname in
    if parent = dir then (
      prerr_endline
        ("Error: cannot find project root containing " ^ bsconfig ^ ".");
      assert false)
    else findProjectRoot ~dir:parent

let setProjectRoot () =
  projectRoot := findProjectRoot ~dir:(Sys.getcwd ());
  bsbProjectRoot :=
    match Sys.getenv_opt "BSB_PROJECT_ROOT" with
    | None -> !projectRoot
    | Some s -> s

let concat = Filename.concat

let handleNamespace cmt =
  let cutAfterDash s =
    match String.index s '-' with
    | n -> String.sub s 0 n
    | exception Not_found -> s
  in
  let noDir = Filename.basename cmt = cmt in
  if noDir then cmt |> Filename.chop_extension |> cutAfterDash
  else
    let dir = cmt |> Filename.dirname in
    let base =
      cmt |> Filename.basename |> Filename.chop_extension |> cutAfterDash
    in
    Filename.concat dir base

let findNameSpace cmt =
  let keepAfterDash s =
    match String.index s '-' with
    | n -> Some (String.sub s (n + 1) (String.length s - n - 1))
    | exception Not_found -> None
  in
  cmt |> Filename.basename |> Filename.chop_extension |> keepAfterDash

let getOutputFileRelative ~config cmt =
  (cmt |> handleNamespace) ^ EmitType.outputFileSuffix ~config

let getOutputFile ~config cmt =
  Filename.concat !projectRoot (getOutputFileRelative ~config cmt)

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
      let pathCmti = Filename.chop_extension pathCmt ^ ".cmti" in
      let pathCmtiLowerCase =
        Filename.chop_extension pathCmtLowerCase ^ ".cmti"
      in
      if Sys.file_exists pathCmtiLowerCase then pathCmtiLowerCase
      else if Sys.file_exists pathCmti then pathCmti
      else if Sys.file_exists pathCmtLowerCase then pathCmtLowerCase
      else if Sys.file_exists pathCmt then pathCmt
      else ""
    else ""
  in
  cmtFile

let getBsConfigFile () =
  let bsconfig = concat !projectRoot "bsconfig.json" in
  match bsconfig |> Sys.file_exists with true -> Some bsconfig | false -> None

(** Find the relative path from /.../bs/lib
   e.g. /foo/bar/bs/lib/src/Hello.re --> src/Hello.re *)
let relativePathFromBsLib fileName =
  if Filename.is_relative fileName then fileName
  else
    let rec pathToList path =
      let isRoot = path |> Filename.basename = path in
      match isRoot with
      | true -> [path]
      | false ->
        (path |> Filename.basename) :: (path |> Filename.dirname |> pathToList)
    in
    let rec fromLibBs ~acc reversedList =
      match reversedList with
      | "bs" :: "lib" :: _ -> acc
      | dir :: dirs -> fromLibBs ~acc:(dir :: acc) dirs
      | [] -> []
    in
    fileName |> pathToList |> fromLibBs ~acc:[] |> fun l ->
    match l with
    | [] -> fileName
    | root :: dirs -> dirs |> List.fold_left concat root

let readConfig ~bsVersion ~namespace =
  setProjectRoot ();
  Config.readConfig ~bsVersion ~getBsConfigFile ~namespace
