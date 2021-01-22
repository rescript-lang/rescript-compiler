
    open J  
    let [@inline] unknown _ x = x
    let [@inline] option sub self = fun v -> 
      match v with 
      | None -> None
      | Some v -> Some (sub self v)
    let rec list sub self = fun x  -> 
      match x with 
      | [] -> []
      | x::xs -> 
         let v = sub self x in 
        v ::list sub self xs

    type iter = {
      label : label fn;
required_modules : required_modules fn;
ident : ident fn;
module_id : module_id fn;
vident : vident fn;
exception_ident : exception_ident fn;
for_ident : for_ident fn;
for_direction : for_direction fn;
property_map : property_map fn;
length_object : length_object fn;
expression_desc : expression_desc fn;
for_ident_expression : for_ident_expression fn;
finish_ident_expression : finish_ident_expression fn;
statement_desc : statement_desc fn;
expression : expression fn;
statement : statement fn;
variable_declaration : variable_declaration fn;
string_clause : string_clause fn;
int_clause : int_clause fn;
case_clause : case_clause fn;
block : block fn;
program : program fn;
deps_program : deps_program fn
    }  
    and 'a fn = iter -> 'a -> 'a
    let super : iter = {
     label : label fn  = ( unknown )  ;
 required_modules : required_modules fn  = ( fun _self arg -> list _self.module_id _self arg )  ;
 ident : ident fn  = ( unknown )  ;
 module_id : module_id fn  = ( fun _self { id = _x0;kind = _x1} -> begin let _x0 = _self.ident _self _x0 in  {id = _x0;kind = _x1} end )  ;
 vident : vident fn  = ( fun _self -> function 
| Id ( _x0)  -> 
 begin let _x0 = _self.ident _self _x0 in  Id ( _x0)  end
|Qualified ( _x0,_x1)  -> 
 begin let _x0 = _self.module_id _self _x0 in  Qualified ( _x0,_x1)  end )  ;
 exception_ident : exception_ident fn  = ( (fun _self arg -> _self.ident _self arg) )  ;
 for_ident : for_ident fn  = ( (fun _self arg -> _self.ident _self arg) )  ;
 for_direction : for_direction fn  = ( unknown )  ;
 property_map : property_map fn  = ( fun _self arg -> list ((fun _self (_x0,_x1) -> begin let _x1 = _self.expression _self _x1 in  (_x0,_x1) end)) _self arg )  ;
 length_object : length_object fn  = ( unknown )  ;
 expression_desc : expression_desc fn  = ( fun _self -> function 
| Length ( _x0,_x1)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = _self.length_object _self _x1 in  Length ( _x0,_x1)  end
|Char_of_int ( _x0)  -> 
 begin let _x0 = _self.expression _self _x0 in  Char_of_int ( _x0)  end
|Char_to_int ( _x0)  -> 
 begin let _x0 = _self.expression _self _x0 in  Char_to_int ( _x0)  end
|Is_null_or_undefined ( _x0)  -> 
 begin let _x0 = _self.expression _self _x0 in  Is_null_or_undefined ( _x0)  end
|String_append ( _x0,_x1)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = _self.expression _self _x1 in  String_append ( _x0,_x1)  end
|Bool _ as v -> v
|Typeof ( _x0)  -> 
 begin let _x0 = _self.expression _self _x0 in  Typeof ( _x0)  end
|Js_not ( _x0)  -> 
 begin let _x0 = _self.expression _self _x0 in  Js_not ( _x0)  end
|Seq ( _x0,_x1)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = _self.expression _self _x1 in  Seq ( _x0,_x1)  end
|Cond ( _x0,_x1,_x2)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = _self.expression _self _x1 in 
let _x2 = _self.expression _self _x2 in  Cond ( _x0,_x1,_x2)  end
|Bin ( _x0,_x1,_x2)  -> 
 begin let _x1 = _self.expression _self _x1 in 
let _x2 = _self.expression _self _x2 in  Bin ( _x0,_x1,_x2)  end
|FlatCall ( _x0,_x1)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = _self.expression _self _x1 in  FlatCall ( _x0,_x1)  end
|Call ( _x0,_x1,_x2)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = list _self.expression _self _x1 in  Call ( _x0,_x1,_x2)  end
|String_index ( _x0,_x1)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = _self.expression _self _x1 in  String_index ( _x0,_x1)  end
|Array_index ( _x0,_x1)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = _self.expression _self _x1 in  Array_index ( _x0,_x1)  end
|Static_index ( _x0,_x1,_x2)  -> 
 begin let _x0 = _self.expression _self _x0 in  Static_index ( _x0,_x1,_x2)  end
|New ( _x0,_x1)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = option (fun _self arg -> list _self.expression _self arg) _self _x1 in  New ( _x0,_x1)  end
|Var ( _x0)  -> 
 begin let _x0 = _self.vident _self _x0 in  Var ( _x0)  end
|Fun ( _x0,_x1,_x2,_x3)  -> 
 begin let _x1 = list _self.ident _self _x1 in 
let _x2 = _self.block _self _x2 in  Fun ( _x0,_x1,_x2,_x3)  end
|Str _ as v -> v
|Unicode _ as v -> v
|Raw_js_code _ as v -> v
|Array ( _x0,_x1)  -> 
 begin let _x0 = list _self.expression _self _x0 in  Array ( _x0,_x1)  end
|Optional_block ( _x0,_x1)  -> 
 begin let _x0 = _self.expression _self _x0 in  Optional_block ( _x0,_x1)  end
|Caml_block ( _x0,_x1,_x2,_x3)  -> 
 begin let _x0 = list _self.expression _self _x0 in 
let _x2 = _self.expression _self _x2 in  Caml_block ( _x0,_x1,_x2,_x3)  end
|Caml_block_tag ( _x0)  -> 
 begin let _x0 = _self.expression _self _x0 in  Caml_block_tag ( _x0)  end
|Number _ as v -> v
|Object ( _x0)  -> 
 begin let _x0 = _self.property_map _self _x0 in  Object ( _x0)  end
|Undefined as v -> v
|Null as v -> v )  ;
 for_ident_expression : for_ident_expression fn  = ( (fun _self arg -> _self.expression _self arg) )  ;
 finish_ident_expression : finish_ident_expression fn  = ( (fun _self arg -> _self.expression _self arg) )  ;
 statement_desc : statement_desc fn  = ( fun _self -> function 
| Block ( _x0)  -> 
 begin let _x0 = _self.block _self _x0 in  Block ( _x0)  end
|Variable ( _x0)  -> 
 begin let _x0 = _self.variable_declaration _self _x0 in  Variable ( _x0)  end
|Exp ( _x0)  -> 
 begin let _x0 = _self.expression _self _x0 in  Exp ( _x0)  end
|If ( _x0,_x1,_x2)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = _self.block _self _x1 in 
let _x2 = _self.block _self _x2 in  If ( _x0,_x1,_x2)  end
|While ( _x0,_x1,_x2,_x3)  -> 
 begin let _x0 = option _self.label _self _x0 in 
let _x1 = _self.expression _self _x1 in 
let _x2 = _self.block _self _x2 in  While ( _x0,_x1,_x2,_x3)  end
|ForRange ( _x0,_x1,_x2,_x3,_x4,_x5)  -> 
 begin let _x0 = option _self.for_ident_expression _self _x0 in 
let _x1 = _self.finish_ident_expression _self _x1 in 
let _x2 = _self.for_ident _self _x2 in 
let _x3 = _self.for_direction _self _x3 in 
let _x4 = _self.block _self _x4 in  ForRange ( _x0,_x1,_x2,_x3,_x4,_x5)  end
|Continue ( _x0)  -> 
 begin let _x0 = _self.label _self _x0 in  Continue ( _x0)  end
|Break as v -> v
|Return ( _x0)  -> 
 begin let _x0 = _self.expression _self _x0 in  Return ( _x0)  end
|Int_switch ( _x0,_x1,_x2)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = list _self.int_clause _self _x1 in 
let _x2 = option _self.block _self _x2 in  Int_switch ( _x0,_x1,_x2)  end
|String_switch ( _x0,_x1,_x2)  -> 
 begin let _x0 = _self.expression _self _x0 in 
let _x1 = list _self.string_clause _self _x1 in 
let _x2 = option _self.block _self _x2 in  String_switch ( _x0,_x1,_x2)  end
|Throw ( _x0)  -> 
 begin let _x0 = _self.expression _self _x0 in  Throw ( _x0)  end
|Try ( _x0,_x1,_x2)  -> 
 begin let _x0 = _self.block _self _x0 in 
let _x1 = option ((fun _self (_x0,_x1) -> begin let _x0 = _self.exception_ident _self _x0 in  let _x1 = _self.block _self _x1 in  (_x0,_x1) end)) _self _x1 in 
let _x2 = option _self.block _self _x2 in  Try ( _x0,_x1,_x2)  end
|Debugger as v -> v )  ;
 expression : expression fn  = ( fun _self { expression_desc = _x0;comment = _x1} -> begin let _x0 = _self.expression_desc _self _x0 in  {expression_desc = _x0;comment = _x1} end )  ;
 statement : statement fn  = ( fun _self { statement_desc = _x0;comment = _x1} -> begin let _x0 = _self.statement_desc _self _x0 in  {statement_desc = _x0;comment = _x1} end )  ;
 variable_declaration : variable_declaration fn  = ( fun _self { ident = _x0;value = _x1;property = _x2;ident_info = _x3} -> begin let _x0 = _self.ident _self _x0 in 
let _x1 = option _self.expression _self _x1 in  {ident = _x0;value = _x1;property = _x2;ident_info = _x3} end )  ;
 string_clause : string_clause fn  = ( (fun _self (_x0,_x1) -> begin let _x1 = _self.case_clause _self _x1 in  (_x0,_x1) end) )  ;
 int_clause : int_clause fn  = ( (fun _self (_x0,_x1) -> begin let _x1 = _self.case_clause _self _x1 in  (_x0,_x1) end) )  ;
 case_clause : case_clause fn  = ( fun _self { switch_body = _x0;should_break = _x1;comment = _x2} -> begin let _x0 = _self.block _self _x0 in  {switch_body = _x0;should_break = _x1;comment = _x2} end )  ;
 block : block fn  = ( fun _self arg -> list _self.statement _self arg )  ;
 program : program fn  = ( fun _self { block = _x0;exports = _x1;export_set = _x2} -> begin let _x0 = _self.block _self _x0 in  {block = _x0;exports = _x1;export_set = _x2} end )  ;
 deps_program : deps_program fn  = ( fun _self { program = _x0;modules = _x1;side_effect = _x2} -> begin let _x0 = _self.program _self _x0 in 
let _x1 = _self.required_modules _self _x1 in  {program = _x0;modules = _x1;side_effect = _x2} end )      
    }
    