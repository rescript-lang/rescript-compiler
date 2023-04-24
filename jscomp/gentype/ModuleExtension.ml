open GenTypeCommon

let shimTsOutputFileExtension ~(config : Config.t) =
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

let tsInputFileSuffix ~(config : Config.t) =
  match config.generatedFileExtension with
  | Some s when Filename.extension s <> "" (* double extension  *) -> s
  | _ -> generatedFilesExtension ~config ^ ".tsx"

let tsOutputFileSuffix ~(config : Config.t) =
  generatedFilesExtension ~config ^ ".js"

let generatedModuleExtension ~(config : Config.t) =
  match config.moduleResolution with
  | Node -> generatedFilesExtension ~config
  | Node16 -> tsOutputFileSuffix ~config
  | Bundler -> tsInputFileSuffix ~config
