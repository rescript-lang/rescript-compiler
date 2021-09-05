open J

let unknown _ _ = ()

let[@inline] option sub self v =
  match v with None -> () | Some v -> sub self v

let rec list sub self x =
  match x with
  | [] -> ()
  | x :: xs ->
      sub self x;
      list sub self xs

type iter = {
  ident : ident fn;
  module_id : module_id fn;
  vident : vident fn;
  exception_ident : exception_ident fn;
  for_ident : for_ident fn;
  expression : expression fn;
  statement : statement fn;
  variable_declaration : variable_declaration fn;
  block : block fn;
  program : program fn;
}

and 'a fn = iter -> 'a -> unit

let label : label fn = unknown

let ident : ident fn = unknown

let module_id : module_id fn =
 fun _self { id = _x0; kind = _x1 } -> _self.ident _self _x0

let required_modules : required_modules fn =
 fun _self arg -> list _self.module_id _self arg

let vident : vident fn =
 fun _self -> function
  | Id _x0 -> _self.ident _self _x0
  | Qualified (_x0, _x1) -> _self.module_id _self _x0

let exception_ident : exception_ident fn =
 fun _self arg -> _self.ident _self arg

let for_ident : for_ident fn = fun _self arg -> _self.ident _self arg

let for_direction : for_direction fn = unknown

let property_map : property_map fn =
 fun _self arg ->
  list (fun _self (_x0, _x1) -> _self.expression _self _x1) _self arg

let length_object : length_object fn = unknown

let expression_desc : expression_desc fn =
 fun _self -> function
  | Length (_x0, _x1) ->
      _self.expression _self _x0;
      length_object _self _x1
  | Is_null_or_undefined _x0 -> _self.expression _self _x0
  | String_append (_x0, _x1) ->
      _self.expression _self _x0;
      _self.expression _self _x1
  | Bool _ -> ()
  | Typeof _x0 -> _self.expression _self _x0
  | Js_not _x0 -> _self.expression _self _x0
  | Seq (_x0, _x1) ->
      _self.expression _self _x0;
      _self.expression _self _x1
  | Cond (_x0, _x1, _x2) ->
      _self.expression _self _x0;
      _self.expression _self _x1;
      _self.expression _self _x2
  | Bin (_x0, _x1, _x2) ->
      _self.expression _self _x1;
      _self.expression _self _x2
  | FlatCall (_x0, _x1) ->
      _self.expression _self _x0;
      _self.expression _self _x1
  | Call (_x0, _x1, _x2) ->
      _self.expression _self _x0;
      list _self.expression _self _x1
  | String_index (_x0, _x1) ->
      _self.expression _self _x0;
      _self.expression _self _x1
  | Array_index (_x0, _x1) ->
      _self.expression _self _x0;
      _self.expression _self _x1
  | Static_index (_x0, _x1, _x2) -> _self.expression _self _x0
  | New (_x0, _x1) ->
      _self.expression _self _x0;
      option (fun _self arg -> list _self.expression _self arg) _self _x1
  | Var _x0 -> _self.vident _self _x0
  | Fun (_x0, _x1, _x2, _x3, _x4) ->
      list _self.ident _self _x1;
      _self.block _self _x2
  | Str _ -> ()
  | Unicode _ -> ()
  | Raw_js_code _ -> ()
  | Array (_x0, _x1) -> list _self.expression _self _x0
  | Optional_block (_x0, _x1) -> _self.expression _self _x0
  | Caml_block (_x0, _x1, _x2, _x3) ->
      list _self.expression _self _x0;
      _self.expression _self _x2
  | Caml_block_tag _x0 -> _self.expression _self _x0
  | Number _ -> ()
  | Object _x0 -> property_map _self _x0
  | Undefined -> ()
  | Null -> ()

let for_ident_expression : for_ident_expression fn =
 fun _self arg -> _self.expression _self arg

let finish_ident_expression : finish_ident_expression fn =
 fun _self arg -> _self.expression _self arg

let case_clause : case_clause fn =
 fun _self { switch_body = _x0; should_break = _x1; comment = _x2 } ->
  _self.block _self _x0

let string_clause : string_clause fn =
 fun _self (_x0, _x1) -> case_clause _self _x1

let int_clause : int_clause fn = fun _self (_x0, _x1) -> case_clause _self _x1

let statement_desc : statement_desc fn =
 fun _self -> function
  | Block _x0 -> _self.block _self _x0
  | Variable _x0 -> _self.variable_declaration _self _x0
  | Exp _x0 -> _self.expression _self _x0
  | If (_x0, _x1, _x2) ->
      _self.expression _self _x0;
      _self.block _self _x1;
      _self.block _self _x2
  | While (_x0, _x1, _x2, _x3) ->
      option label _self _x0;
      _self.expression _self _x1;
      _self.block _self _x2
  | ForRange (_x0, _x1, _x2, _x3, _x4, _x5) ->
      option for_ident_expression _self _x0;
      finish_ident_expression _self _x1;
      _self.for_ident _self _x2;
      for_direction _self _x3;
      _self.block _self _x4
  | Continue _x0 -> label _self _x0
  | Break -> ()
  | Return _x0 -> _self.expression _self _x0
  | Int_switch (_x0, _x1, _x2) ->
      _self.expression _self _x0;
      list int_clause _self _x1;
      option _self.block _self _x2
  | String_switch (_x0, _x1, _x2) ->
      _self.expression _self _x0;
      list string_clause _self _x1;
      option _self.block _self _x2
  | Throw _x0 -> _self.expression _self _x0
  | Try (_x0, _x1, _x2) ->
      _self.block _self _x0;
      option
        (fun _self (_x0, _x1) ->
          _self.exception_ident _self _x0;
          _self.block _self _x1)
        _self _x1;
      option _self.block _self _x2
  | Debugger -> ()

let expression : expression fn =
 fun _self { expression_desc = _x0; comment = _x1 } -> expression_desc _self _x0

let statement : statement fn =
 fun _self { statement_desc = _x0; comment = _x1 } -> statement_desc _self _x0

let variable_declaration : variable_declaration fn =
 fun _self { ident = _x0; value = _x1; property = _x2; ident_info = _x3 } ->
  _self.ident _self _x0;
  option _self.expression _self _x1

let block : block fn = fun _self arg -> list _self.statement _self arg

let program : program fn =
 fun _self { block = _x0; exports = _x1; export_set = _x2 } ->
  _self.block _self _x0

let deps_program : deps_program fn =
 fun _self { program = _x0; modules = _x1; side_effect = _x2 } ->
  _self.program _self _x0;
  required_modules _self _x1

let super : iter =
  {
    ident;
    module_id;
    vident;
    exception_ident;
    for_ident;
    expression;
    statement;
    variable_declaration;
    block;
    program;
  }
