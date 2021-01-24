
    open J  
    let [@inline] unknown _self _ = _self
    let [@inline] option sub  self = fun v -> 
      match v with 
      | None -> self 
      | Some x -> sub self x 
    let rec list (sub : 'self_type -> 'a -> 'self_type) self = fun v ->
      match  v with 
      | [] -> self 
      | x::xs -> 
        let self = sub self x in 
        list sub self xs 
    class  fold =
      object ((_self : 'self_type))
        method list :
          'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
          fun _f_a ->
            function
            | [] -> _self
            | _x :: _x_i1 -> let _self = _f_a _self _x in let _self = _self#list _f_a _x_i1 in _self
    method label : label -> 'self_type = unknown _self  
method ident : ident -> 'self_type = unknown _self  
method module_id : module_id -> 'self_type = fun { id = _x0;kind = _x1} -> let _self = _self#ident _x0 in _self  
method required_modules : required_modules -> 'self_type = list (fun _self -> _self#module_id) _self  
method vident : vident -> 'self_type = function 
| Id ( _x0)  -> 
let _self = _self#ident _x0 in
 _self
|Qualified ( _x0,_x1)  -> 
let _self = _self#module_id _x0 in
 _self  
method exception_ident : exception_ident -> 'self_type = _self#ident  
method for_ident : for_ident -> 'self_type = _self#ident  
method for_direction : for_direction -> 'self_type = unknown _self  
method property_map : property_map -> 'self_type = list (fun _self -> fun ( _x0,_x1) -> let _self = _self#expression _x1 in _self) _self  
method length_object : length_object -> 'self_type = unknown _self  
method expression_desc : expression_desc -> 'self_type = function 
| Length ( _x0,_x1)  -> 
let _self = _self#expression _x0 in
let _self = _self#length_object _x1 in
 _self
|Char_of_int ( _x0)  -> 
let _self = _self#expression _x0 in
 _self
|Char_to_int ( _x0)  -> 
let _self = _self#expression _x0 in
 _self
|Is_null_or_undefined ( _x0)  -> 
let _self = _self#expression _x0 in
 _self
|String_append ( _x0,_x1)  -> 
let _self = _self#expression _x0 in
let _self = _self#expression _x1 in
 _self
|Bool _ -> _self
|Typeof ( _x0)  -> 
let _self = _self#expression _x0 in
 _self
|Js_not ( _x0)  -> 
let _self = _self#expression _x0 in
 _self
|Seq ( _x0,_x1)  -> 
let _self = _self#expression _x0 in
let _self = _self#expression _x1 in
 _self
|Cond ( _x0,_x1,_x2)  -> 
let _self = _self#expression _x0 in
let _self = _self#expression _x1 in
let _self = _self#expression _x2 in
 _self
|Bin ( _x0,_x1,_x2)  -> 
let _self = _self#expression _x1 in
let _self = _self#expression _x2 in
 _self
|FlatCall ( _x0,_x1)  -> 
let _self = _self#expression _x0 in
let _self = _self#expression _x1 in
 _self
|Call ( _x0,_x1,_x2)  -> 
let _self = _self#expression _x0 in
let _self = list (fun _self -> _self#expression) _self _x1 in
 _self
|String_index ( _x0,_x1)  -> 
let _self = _self#expression _x0 in
let _self = _self#expression _x1 in
 _self
|Array_index ( _x0,_x1)  -> 
let _self = _self#expression _x0 in
let _self = _self#expression _x1 in
 _self
|Static_index ( _x0,_x1,_x2)  -> 
let _self = _self#expression _x0 in
 _self
|New ( _x0,_x1)  -> 
let _self = _self#expression _x0 in
let _self = option (fun _self -> list (fun _self -> _self#expression) _self) _self _x1 in
 _self
|Var ( _x0)  -> 
let _self = _self#vident _x0 in
 _self
|Fun ( _x0,_x1,_x2,_x3)  -> 
let _self = list (fun _self -> _self#ident) _self _x1 in
let _self = _self#block _x2 in
 _self
|Str _ -> _self
|Unicode _ -> _self
|Raw_js_code _ -> _self
|Array ( _x0,_x1)  -> 
let _self = list (fun _self -> _self#expression) _self _x0 in
 _self
|Optional_block ( _x0,_x1)  -> 
let _self = _self#expression _x0 in
 _self
|Caml_block ( _x0,_x1,_x2,_x3)  -> 
let _self = list (fun _self -> _self#expression) _self _x0 in
let _self = _self#expression _x2 in
 _self
|Caml_block_tag ( _x0)  -> 
let _self = _self#expression _x0 in
 _self
|Number _ -> _self
|Object ( _x0)  -> 
let _self = _self#property_map _x0 in
 _self
|Undefined -> _self 
|Null -> _self   
method for_ident_expression : for_ident_expression -> 'self_type = _self#expression  
method finish_ident_expression : finish_ident_expression -> 'self_type = _self#expression  
method case_clause : case_clause -> 'self_type = fun { switch_body = _x0;should_break = _x1;comment = _x2} -> let _self = _self#block _x0 in _self  
method string_clause : string_clause -> 'self_type = fun ( _x0,_x1) -> let _self = _self#case_clause _x1 in _self  
method int_clause : int_clause -> 'self_type = fun ( _x0,_x1) -> let _self = _self#case_clause _x1 in _self  
method statement_desc : statement_desc -> 'self_type = function 
| Block ( _x0)  -> 
let _self = _self#block _x0 in
 _self
|Variable ( _x0)  -> 
let _self = _self#variable_declaration _x0 in
 _self
|Exp ( _x0)  -> 
let _self = _self#expression _x0 in
 _self
|If ( _x0,_x1,_x2)  -> 
let _self = _self#expression _x0 in
let _self = _self#block _x1 in
let _self = _self#block _x2 in
 _self
|While ( _x0,_x1,_x2,_x3)  -> 
let _self = option (fun _self -> _self#label) _self _x0 in
let _self = _self#expression _x1 in
let _self = _self#block _x2 in
 _self
|ForRange ( _x0,_x1,_x2,_x3,_x4,_x5)  -> 
let _self = option (fun _self -> _self#for_ident_expression) _self _x0 in
let _self = _self#finish_ident_expression _x1 in
let _self = _self#for_ident _x2 in
let _self = _self#for_direction _x3 in
let _self = _self#block _x4 in
 _self
|Continue ( _x0)  -> 
let _self = _self#label _x0 in
 _self
|Break -> _self 
|Return ( _x0)  -> 
let _self = _self#expression _x0 in
 _self
|Int_switch ( _x0,_x1,_x2)  -> 
let _self = _self#expression _x0 in
let _self = list (fun _self -> _self#int_clause) _self _x1 in
let _self = option (fun _self -> _self#block) _self _x2 in
 _self
|String_switch ( _x0,_x1,_x2)  -> 
let _self = _self#expression _x0 in
let _self = list (fun _self -> _self#string_clause) _self _x1 in
let _self = option (fun _self -> _self#block) _self _x2 in
 _self
|Throw ( _x0)  -> 
let _self = _self#expression _x0 in
 _self
|Try ( _x0,_x1,_x2)  -> 
let _self = _self#block _x0 in
let _self = option (fun _self -> fun ( _x0,_x1) -> let _self = _self#exception_ident _x0 in let _self = _self#block _x1 in _self) _self _x1 in
let _self = option (fun _self -> _self#block) _self _x2 in
 _self
|Debugger -> _self   
method expression : expression -> 'self_type = fun { expression_desc = _x0;comment = _x1} -> let _self = _self#expression_desc _x0 in _self  
method statement : statement -> 'self_type = fun { statement_desc = _x0;comment = _x1} -> let _self = _self#statement_desc _x0 in _self  
method variable_declaration : variable_declaration -> 'self_type = fun { ident = _x0;value = _x1;property = _x2;ident_info = _x3} -> let _self = _self#ident _x0 in
let _self = option (fun _self -> _self#expression) _self _x1 in _self  
method block : block -> 'self_type = list (fun _self -> _self#statement) _self  
method program : program -> 'self_type = fun { block = _x0;exports = _x1;export_set = _x2} -> let _self = _self#block _x0 in _self  
method deps_program : deps_program -> 'self_type = fun { program = _x0;modules = _x1;side_effect = _x2} -> let _self = _self#program _x0 in
let _self = _self#required_modules _x1 in _self      
    end
    