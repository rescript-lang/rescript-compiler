module C = Stack
val read_parse_and_extract :
    'a -> (Depend.StringSet.t -> 'a -> 'b) -> Depend.StringSet.t
type file_kind = ML | MLI
val files :
    (Depend.StringSet.elt * file_kind * Depend.StringSet.t) list ref
val ml_file_dependencies : string * Parsetree.structure -> unit
val mli_file_dependencies :
    Depend.StringSet.elt * Parsetree.signature -> unit
val normalize : string -> string
val merge :
    (string * file_kind * Depend.StringSet.t) list ->
      (string, Depend.StringSet.t) Hashtbl.t
val sort_files_by_dependencies :
    (string * file_kind * Depend.StringSet.t) list ->
      Depend.StringSet.elt C.t

val process : string list -> Parsetree.structure_item

val process_as_string : string list ->  [`All of string * string * string * string |`Ml of string * string ] list 
