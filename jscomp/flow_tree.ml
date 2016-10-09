type prog = {
  prog_types: decl list;
  prog_exports: decl list;
}

and decl = {
  decl_name: id;
  decl_type: type_;
  decl_tvars: int list;
}

and type_ =
  | T_name of id * type_ list * string option
  | T_tvar of int
  | T_fun of func
  | T_obj of field list
  | T_tuple of type_ list

and id = {
  mutable id: string;
}

and func = {
  func_tvars: int list;
  func_args: type_ list;
  func_ret: type_;
}

and field = {
  field_name: string;
  field_type: type_;
}
