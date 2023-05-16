type package_spec = {
  format : Ext_module_system.t;
  in_source : bool;
  suffix : Ext_js_suffix.t;
}

type ppx_spec = {
  name : string;
  args : string list;
}

module Warning = struct
  type error =
    | Warn_error_false
    (* default [false] to make our changes non-intrusive *)
    | Warn_error_true
    | Warn_error_number of string

  type t = {
    number : string option;
    error : error;
  }
end

module ReasonReact = struct
  type jsx_version = Jsx_v3
  type t = {
    react_jsx : jsx_version;
  }
end

module Jsx = struct
  type dependencies = string list
  type version = Jsx_v3 | Jsx_v4
  type module_ = React
  type mode = Classic | Automatic
  type t = {
    version : version option;
    module_ : module_ option;
    mode : mode option;
    v3_dependencies : dependencies;
  }
end

module Gentype = struct
  type module_ = CommonJS | ES6

  (** Compatibility for `compilerOptions.moduleResolution` in TypeScript projects. *)
  type moduleResolution =
    | Node  (** should drop extension on import statements *)
    | Node16
        (** should use TS output's extension (e.g. `.gen.js`) on import statements *)
    | Bundler
        (** should use TS input's extension (e.g. `.gen.tsx`) on import statements *)

  type t = {
    module_ : module_;
    moduleResolution : moduleResolution;
    exportInterfaces : bool;
    generatedFileExtension : string option;
    shims : string Map_string.t;
    debug : Ext_json_types.t Map_string.t option;
  }
end

type t = {
  package_name : string;
  namespace : string option;
  external_includes : string list;
  bsc_flags : string list;
  ppx_specs : ppx_spec list;
  pp_file : string option;
  bs_dependencies : Set_string.t;
  bs_dev_dependencies : Set_string.t;
  pinned_dependencies : Set_string.t;
  warning : Warning.t option;
  js_post_build_cmd : string option;
  package_specs : package_spec list;
  suffix : Ext_js_suffix.t;
  reason_react : ReasonReact.t option;
  jsx: Jsx.t;
  generators : string Map_string.t;
  cut_generators : bool;
  uncurried : bool;
  ignored_dirs : string list;
  use_stdlib : bool;
  external_stdlib : string option;
  gentype : Gentype.t option;
}
