open GenTypeCommon

let shimExtension ~(config : Config.t) =
  match config.moduleResolution with
  | Node -> ".shim"
  | Node16 -> ".shim.js"
  | Bundler -> ".shim.ts"

let generatedFilesExtension ~(config : Config.t) =
  match config.generatedFileExtension with
  | Some s ->
    (* from .foo.bar to .foo *)
    Filename.remove_extension s
  | None -> ".gen"

let inputFileSuffix ~(config : Config.t) =
  match config.generatedFileExtension with
  | Some s when Filename.extension s <> "" (* double extension  *) -> s
  | _ -> generatedFilesExtension ~config ^ ".tsx"

let outputFileSuffix ~(config : Config.t) =
  generatedFilesExtension ~config ^ ".js"

let generatedModuleExtension ~(config : Config.t) =
  match config.moduleResolution with
  | Node -> generatedFilesExtension ~config
  | Node16 -> inputFileSuffix ~config
  | Bundler -> outputFileSuffix ~config
