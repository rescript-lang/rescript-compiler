type prog = {
  prog_types: decl list;
  prog_exports: decl list;
}

and decl = {
  decl_name: string;
  decl_type: type_;
}

and type_ =
  | T_name of string * type_ list * string option
  | T_fun of arg list * type_
  | T_obj of field list
  | T_tuple of type_ list

and arg = type_

and field = {
  field_name: string;
  field_type: type_;
}
