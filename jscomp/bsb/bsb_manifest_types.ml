module SourceItem = struct
  type type_ = Dev
  type public = Export_none | Export_all | Export_set of Set_string.t

  type source =
    | Path_set of Set_string.t
    | Predicate of { regex : string; excludes : string list }

  type subdir = Recursive_none | Recursive_all | Recursive_source of source

  type generator = {
    name : string;
    edge : string;
  }

  type t = {
    dir : string;
    type_ : type_ option;
    files : source option;
    generators : generator list;
    public : public;
    resources : string list;
    subdir : subdir;
  }

  let from_string dir =
    {
      dir;
      type_ = None;
      files = None;
      generators = [];
      public = Export_all;
      resources = [];
      subdir = Recursive_all;
    }

  let from_string_array dirs =
    Ext_list.map dirs from_string
end

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
  ppx_flags: ppx_spec list;
  pp_flags: string option;
  bs_dependencies : string list;
  bs_dev_dependencies : string list;
  pinned_dependencies : Set_string.t;
  warning : Warning.t option;
  js_post_build_cmd : string option;
  sources : SourceItem.t list;
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
