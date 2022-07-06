open GenTypeCommon

type t = string * string

let bsCurryPath ~config = ("", Config_.getBsCurryPath ~config)

let fromModule ~dir ~importExtension moduleName =
  let withNoPath =
    (moduleName |> ModuleName.toString |> ScopedPackage.removeGeneratedModule)
    ^ importExtension
  in
  (dir, withNoPath)

let fromStringUnsafe s = ("", s)

let chopExtensionSafe s =
  try s |> Filename.chop_extension with Invalid_argument _ -> s

let dump (dir, s) = NodeFilename.concat dir s

let toCmt ~config ~outputFileRelative (dir, s) =
  let open Filename in
  concat
    (outputFileRelative |> dirname)
    (((dir, s |> chopExtensionSafe) |> dump)
    ^ (match config.namespace with None -> "" | Some name -> "-" ^ name)
    ^ ".cmt")

let emit (dir, s) = (dir, s) |> dump
