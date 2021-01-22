
    open J  
    let unknown _ _ = ()
    let option sub self = fun v -> 
      match v with 
      | None -> ()
      | Some v -> sub self v
    let rec list sub self = fun x  -> 
      match x with 
      | [] -> ()
      | x::xs -> 
         sub self x ;
        list sub self xs

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
    and 'a fn = iter -> 'a -> unit
    let iter : iter = {
     label : label fn  = ( unknown )  ;
 required_modules : required_modules fn  = ( fun _self arg -> list _self.module_id _self arg )  ;
 ident : ident fn  = ( unknown )  ;
 module_id : module_id fn  = ( fun _self { id = _x0;kind = _x1} -> begin _self.ident _self _x0 end )  ;
 vident : vident fn  = ( fun _self -> function 
| Id ( _x0)  -> 
 begin _self.ident _self _x0 end
|Qualified ( _x0,_x1)  -> 
 begin _self.module_id _self _x0 end )  ;
 exception_ident : exception_ident fn  = ( (fun _self arg -> _self.ident _self arg) )  ;
 for_ident : for_ident fn  = ( (fun _self arg -> _self.ident _self arg) )  ;
 for_direction : for_direction fn  = ( unknown )  ;
 property_map : property_map fn  = ( fun _self arg -> list ((fun _self (_x0,_x1) -> begin _self.expression _self _x1 end)) _self arg )  ;
 length_object : length_object fn  = ( unknown )  ;
 expression_desc : expression_desc fn  = ( fun _self -> function 
| Length ( _x0,_x1)  -> 
 begin _self.expression _self _x0;_self.length_object _self _x1 end
|Char_of_int ( _x0)  -> 
 begin _self.expression _self _x0 end
|Char_to_int ( _x0)  -> 
 begin _self.expression _self _x0 end
|Is_null_or_undefined ( _x0)  -> 
 begin _self.expression _self _x0 end
|String_append ( _x0,_x1)  -> 
 begin _self.expression _self _x0;_self.expression _self _x1 end
|Bool _ -> ()
|Typeof ( _x0)  -> 
 begin _self.expression _self _x0 end
|Js_not ( _x0)  -> 
 begin _self.expression _self _x0 end
|Seq ( _x0,_x1)  -> 
 begin _self.expression _self _x0;_self.expression _self _x1 end
|Cond ( _x0,_x1,_x2)  -> 
 begin _self.expression _self _x0;_self.expression _self _x1;_self.expression _self _x2 end
|Bin ( _x0,_x1,_x2)  -> 
 begin _self.expression _self _x1;_self.expression _self _x2 end
|FlatCall ( _x0,_x1)  -> 
 begin _self.expression _self _x0;_self.expression _self _x1 end
|Call ( _x0,_x1,_x2)  -> 
 begin _self.expression _self _x0;list _self.expression _self _x1 end
|String_index ( _x0,_x1)  -> 
 begin _self.expression _self _x0;_self.expression _self _x1 end
|Array_index ( _x0,_x1)  -> 
 begin _self.expression _self _x0;_self.expression _self _x1 end
|Static_index ( _x0,_x1,_x2)  -> 
 begin _self.expression _self _x0 end
|New ( _x0,_x1)  -> 
 begin _self.expression _self _x0;option (fun _self arg -> list _self.expression _self arg) _self _x1 end
|Var ( _x0)  -> 
 begin _self.vident _self _x0 end
|Fun ( _x0,_x1,_x2,_x3)  -> 
 begin list _self.ident _self _x1;_self.block _self _x2 end
|Str _ -> ()
|Unicode _ -> ()
|Raw_js_code _ -> ()
|Array ( _x0,_x1)  -> 
 begin list _self.expression _self _x0 end
|Optional_block ( _x0,_x1)  -> 
 begin _self.expression _self _x0 end
|Caml_block ( _x0,_x1,_x2,_x3)  -> 
 begin list _self.expression _self _x0;_self.expression _self _x2 end
|Caml_block_tag ( _x0)  -> 
 begin _self.expression _self _x0 end
|Number _ -> ()
|Object ( _x0)  -> 
 begin _self.property_map _self _x0 end
|Undefined -> ()
|Null -> () )  ;
 for_ident_expression : for_ident_expression fn  = ( (fun _self arg -> _self.expression _self arg) )  ;
 finish_ident_expression : finish_ident_expression fn  = ( (fun _self arg -> _self.expression _self arg) )  ;
 statement_desc : statement_desc fn  = ( fun _self -> function 
| Block ( _x0)  -> 
 begin _self.block _self _x0 end
|Variable ( _x0)  -> 
 begin _self.variable_declaration _self _x0 end
|Exp ( _x0)  -> 
 begin _self.expression _self _x0 end
|If ( _x0,_x1,_x2)  -> 
 begin _self.expression _self _x0;_self.block _self _x1;_self.block _self _x2 end
|While ( _x0,_x1,_x2,_x3)  -> 
 begin option _self.label _self _x0;_self.expression _self _x1;_self.block _self _x2 end
|ForRange ( _x0,_x1,_x2,_x3,_x4,_x5)  -> 
 begin option _self.for_ident_expression _self _x0;_self.finish_ident_expression _self _x1;_self.for_ident _self _x2;_self.for_direction _self _x3;_self.block _self _x4 end
|Continue ( _x0)  -> 
 begin _self.label _self _x0 end
|Break -> ()
|Return ( _x0)  -> 
 begin _self.expression _self _x0 end
|Int_switch ( _x0,_x1,_x2)  -> 
 begin _self.expression _self _x0;list _self.int_clause _self _x1;option _self.block _self _x2 end
|String_switch ( _x0,_x1,_x2)  -> 
 begin _self.expression _self _x0;list _self.string_clause _self _x1;option _self.block _self _x2 end
|Throw ( _x0)  -> 
 begin _self.expression _self _x0 end
|Try ( _x0,_x1,_x2)  -> 
 begin _self.block _self _x0;option ((fun _self (_x0,_x1) -> begin _self.exception_ident _self _x0;_self.block _self _x1 end)) _self _x1;option _self.block _self _x2 end
|Debugger -> () )  ;
 expression : expression fn  = ( fun _self { expression_desc = _x0;comment = _x1} -> begin _self.expression_desc _self _x0 end )  ;
 statement : statement fn  = ( fun _self { statement_desc = _x0;comment = _x1} -> begin _self.statement_desc _self _x0 end )  ;
 variable_declaration : variable_declaration fn  = ( fun _self { ident = _x0;value = _x1;property = _x2;ident_info = _x3} -> begin _self.ident _self _x0;option _self.expression _self _x1 end )  ;
 string_clause : string_clause fn  = ( (fun _self (_x0,_x1) -> begin _self.case_clause _self _x1 end) )  ;
 int_clause : int_clause fn  = ( (fun _self (_x0,_x1) -> begin _self.case_clause _self _x1 end) )  ;
 case_clause : case_clause fn  = ( fun _self { switch_body = _x0;should_break = _x1;comment = _x2} -> begin _self.block _self _x0 end )  ;
 block : block fn  = ( fun _self arg -> list _self.statement _self arg )  ;
 program : program fn  = ( fun _self { block = _x0;exports = _x1;export_set = _x2} -> begin _self.block _self _x0 end )  ;
 deps_program : deps_program fn  = ( fun _self { program = _x0;modules = _x1;side_effect = _x2} -> begin _self.program _self _x0;_self.required_modules _self _x1 end )      
    }
    