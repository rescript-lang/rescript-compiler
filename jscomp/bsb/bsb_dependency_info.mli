type path = string
(**
  Data structure used to track information about the project's dependencies.
  Used by the packing / linking step.
 *)
type t = {
  mutable all_external_deps: path list;
}

