
    open J  

    let option sub  v =
      match v with 
      | None -> ()
      | Some v -> sub  v
    let rec list sub v =
      match v with 
      | [] -> ()
      | x::xs -> sub x ; list sub xs 
    class iter = object (_self : 'self_type)
    method label : label -> unit = ignore  
method required_modules : required_modules -> unit = (list _self#module_id)  
method ident : ident -> unit = ignore  
method module_id : module_id -> unit = fun { id = _x0;kind = _x1} -> begin _self#ident _x0 end  
method vident : vident -> unit = function 
| Id ( _x0)  -> 
 begin _self#ident _x0 end
|Qualified ( _x0,_x1)  -> 
 begin _self#module_id _x0 end  
method exception_ident : exception_ident -> unit = _self#ident  
method for_ident : for_ident -> unit = _self#ident  
method for_direction : for_direction -> unit = ignore  
method property_map : property_map -> unit = (list (fun ( _x0,_x1) -> begin _self#expression _x1 end))  
method length_object : length_object -> unit = ignore  
method expression_desc : expression_desc -> unit = function 
| Length ( _x0,_x1)  -> 
 begin _self#expression _x0;_self#length_object _x1 end
|Char_of_int ( _x0)  -> 
 begin _self#expression _x0 end
|Char_to_int ( _x0)  -> 
 begin _self#expression _x0 end
|Is_null_or_undefined ( _x0)  -> 
 begin _self#expression _x0 end
|String_append ( _x0,_x1)  -> 
 begin _self#expression _x0;_self#expression _x1 end
|Bool _ -> ()
|Typeof ( _x0)  -> 
 begin _self#expression _x0 end
|Js_not ( _x0)  -> 
 begin _self#expression _x0 end
|Seq ( _x0,_x1)  -> 
 begin _self#expression _x0;_self#expression _x1 end
|Cond ( _x0,_x1,_x2)  -> 
 begin _self#expression _x0;_self#expression _x1;_self#expression _x2 end
|Bin ( _x0,_x1,_x2)  -> 
 begin _self#expression _x1;_self#expression _x2 end
|FlatCall ( _x0,_x1)  -> 
 begin _self#expression _x0;_self#expression _x1 end
|Call ( _x0,_x1,_x2)  -> 
 begin _self#expression _x0;(list _self#expression) _x1 end
|String_index ( _x0,_x1)  -> 
 begin _self#expression _x0;_self#expression _x1 end
|Array_index ( _x0,_x1)  -> 
 begin _self#expression _x0;_self#expression _x1 end
|Static_index ( _x0,_x1,_x2)  -> 
 begin _self#expression _x0 end
|New ( _x0,_x1)  -> 
 begin _self#expression _x0;(option (list _self#expression)) _x1 end
|Var ( _x0)  -> 
 begin _self#vident _x0 end
|Fun ( _x0,_x1,_x2,_x3)  -> 
 begin (list _self#ident) _x1;_self#block _x2 end
|Str _ -> ()
|Unicode _ -> ()
|Raw_js_code _ -> ()
|Array ( _x0,_x1)  -> 
 begin (list _self#expression) _x0 end
|Optional_block ( _x0,_x1)  -> 
 begin _self#expression _x0 end
|Caml_block ( _x0,_x1,_x2,_x3)  -> 
 begin (list _self#expression) _x0;_self#expression _x2 end
|Caml_block_tag ( _x0)  -> 
 begin _self#expression _x0 end
|Number _ -> ()
|Object ( _x0)  -> 
 begin _self#property_map _x0 end
|Undefined -> ()
|Null -> ()  
method for_ident_expression : for_ident_expression -> unit = _self#expression  
method finish_ident_expression : finish_ident_expression -> unit = _self#expression  
method statement_desc : statement_desc -> unit = function 
| Block ( _x0)  -> 
 begin _self#block _x0 end
|Variable ( _x0)  -> 
 begin _self#variable_declaration _x0 end
|Exp ( _x0)  -> 
 begin _self#expression _x0 end
|If ( _x0,_x1,_x2)  -> 
 begin _self#expression _x0;_self#block _x1;_self#block _x2 end
|While ( _x0,_x1,_x2,_x3)  -> 
 begin (option _self#label) _x0;_self#expression _x1;_self#block _x2 end
|ForRange ( _x0,_x1,_x2,_x3,_x4,_x5)  -> 
 begin (option _self#for_ident_expression) _x0;_self#finish_ident_expression _x1;_self#for_ident _x2;_self#for_direction _x3;_self#block _x4 end
|Continue ( _x0)  -> 
 begin _self#label _x0 end
|Break -> ()
|Return ( _x0)  -> 
 begin _self#expression _x0 end
|Int_switch ( _x0,_x1,_x2)  -> 
 begin _self#expression _x0;(list _self#int_clause) _x1;(option _self#block) _x2 end
|String_switch ( _x0,_x1,_x2)  -> 
 begin _self#expression _x0;(list _self#string_clause) _x1;(option _self#block) _x2 end
|Throw ( _x0)  -> 
 begin _self#expression _x0 end
|Try ( _x0,_x1,_x2)  -> 
 begin _self#block _x0;(option (fun ( _x0,_x1) -> begin _self#exception_ident _x0;_self#block _x1 end)) _x1;(option _self#block) _x2 end
|Debugger -> ()  
method expression : expression -> unit = fun { expression_desc = _x0;comment = _x1} -> begin _self#expression_desc _x0 end  
method statement : statement -> unit = fun { statement_desc = _x0;comment = _x1} -> begin _self#statement_desc _x0 end  
method variable_declaration : variable_declaration -> unit = fun { ident = _x0;value = _x1;property = _x2;ident_info = _x3} -> begin _self#ident _x0;(option _self#expression) _x1 end  
method string_clause : string_clause -> unit = (fun ( _x0,_x1) -> begin _self#case_clause _x1 end)  
method int_clause : int_clause -> unit = (fun ( _x0,_x1) -> begin _self#case_clause _x1 end)  
method case_clause : case_clause -> unit = fun { switch_body = _x0;should_break = _x1;comment = _x2} -> begin _self#block _x0 end  
method block : block -> unit = (list _self#statement)  
method program : program -> unit = fun { block = _x0;exports = _x1;export_set = _x2} -> begin _self#block _x0 end  
method deps_program : deps_program -> unit = fun { program = _x0;modules = _x1;side_effect = _x2} -> begin _self#program _x0;_self#required_modules _x1 end      
    end
    