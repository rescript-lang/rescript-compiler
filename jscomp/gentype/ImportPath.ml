open GenTypeCommon

type t = string * string

let bsCurryPath ~config = ("", Config.getBsCurryPath ~config)

let fromModule ~dir ~importExtension moduleName =
  let withNoPath =
    (moduleName |> ModuleName.toString |> ScopedPackage.removeGeneratedModule)
    ^ importExtension
  in
  (dir, withNoPath)

let fromStringUnsafe s = ("", s)

let chopExtensionSafe (dir, s) =
  try (dir, s |> Filename.chop_extension) with Invalid_argument _ -> (dir, s)

let dump (dir, s) = NodeFilename.concat dir s

let toCmt ~(config : Config.t) ~outputFileRelative (dir, s) =
  let open Filename in
  concat (outputFileRelative |> dirname) ((dir, s) |> chopExtensionSafe |> dump)
  ^ (match config.namespace with
    | None -> ""
    | Some name -> "-" ^ name)
  ^ ".cmt"

let emit (dir, s) = (dir, s) |> dump
