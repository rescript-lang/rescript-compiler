
    open J  
    class iter =
      object ((o : 'self_type))
        method unknown : 'a. 'a -> unit = ignore
        method option :
          'a. ('self_type -> 'a -> unit) -> 'a option -> unit =
          fun _f_a -> function | None -> () | Some _x ->  _f_a o _x 
        method list :
          'a. ('self_type -> 'a -> unit) -> 'a list -> unit =
          fun _f_a ->
            function
            | [] -> ()
            | _x :: _x_i1 -> _f_a o _x ;  o#list _f_a _x_i1 
    method label : label -> unit = o#unknown  
method required_modules : required_modules -> unit = o#list (fun o -> o#module_id)  
method ident : ident -> unit = o#unknown  
method module_id : module_id -> unit = fun { id = _x0;kind = _x1} -> begin o#ident _x0 end  
method vident : vident -> unit = function 
| Id ( _x0)  -> 
 begin o#ident _x0  end
|Qualified ( _x0,_x1)  -> 
 begin o#module_id _x0 ;o#option (fun o -> o#unknown) _x1  end  
method exception_ident : exception_ident -> unit = o#ident  
method for_ident : for_ident -> unit = o#ident  
method for_direction : for_direction -> unit = o#unknown  
method property_map : property_map -> unit = o#list (fun o -> fun ( _x0,_x1) -> begin o#expression _x1 end)  
method length_object : length_object -> unit = o#unknown  
method expression_desc : expression_desc -> unit = function 
| Length ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#length_object _x1  end
|Char_of_int ( _x0)  -> 
 begin o#expression _x0  end
|Char_to_int ( _x0)  -> 
 begin o#expression _x0  end
|Is_null_or_undefined ( _x0)  -> 
 begin o#expression _x0  end
|String_append ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#expression _x1  end
|Bool ( _x0)  -> 
 begin  end
|Typeof ( _x0)  -> 
 begin o#expression _x0  end
|Js_not ( _x0)  -> 
 begin o#expression _x0  end
|Seq ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#expression _x1  end
|Cond ( _x0,_x1,_x2)  -> 
 begin o#expression _x0 ;o#expression _x1 ;o#expression _x2  end
|Bin ( _x0,_x1,_x2)  -> 
 begin o#expression _x1 ;o#expression _x2  end
|FlatCall ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#expression _x1  end
|Call ( _x0,_x1,_x2)  -> 
 begin o#expression _x0 ;o#list (fun o -> o#expression) _x1  end
|String_index ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#expression _x1  end
|Array_index ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#expression _x1  end
|Static_index ( _x0,_x1,_x2)  -> 
 begin o#expression _x0 ;o#option (fun o -> o#unknown) _x2  end
|New ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#option (fun o -> o#list (fun o -> o#expression)) _x1  end
|Var ( _x0)  -> 
 begin o#vident _x0  end
|Fun ( _x0,_x1,_x2,_x3)  -> 
 begin o#list (fun o -> o#ident) _x1 ;o#block _x2  end
|Str ( _x0,_x1)  -> 
 begin  end
|Unicode ( _x0)  -> 
 begin  end
|Raw_js_code ( _x0)  -> 
 begin  end
|Array ( _x0,_x1)  -> 
 begin o#list (fun o -> o#expression) _x0  end
|Optional_block ( _x0,_x1)  -> 
 begin o#expression _x0  end
|Caml_block ( _x0,_x1,_x2,_x3)  -> 
 begin o#list (fun o -> o#expression) _x0 ;o#expression _x2  end
|Caml_block_tag ( _x0)  -> 
 begin o#expression _x0  end
|Number ( _x0)  -> 
 begin  end
|Object ( _x0)  -> 
 begin o#property_map _x0  end
|Undefined -> ()
|Null -> ()  
method for_ident_expression : for_ident_expression -> unit = o#expression  
method finish_ident_expression : finish_ident_expression -> unit = o#expression  
method statement_desc : statement_desc -> unit = function 
| Block ( _x0)  -> 
 begin o#block _x0  end
|Variable ( _x0)  -> 
 begin o#variable_declaration _x0  end
|Exp ( _x0)  -> 
 begin o#expression _x0  end
|If ( _x0,_x1,_x2)  -> 
 begin o#expression _x0 ;o#block _x1 ;o#block _x2  end
|While ( _x0,_x1,_x2,_x3)  -> 
 begin o#option (fun o -> o#label) _x0 ;o#expression _x1 ;o#block _x2  end
|ForRange ( _x0,_x1,_x2,_x3,_x4,_x5)  -> 
 begin o#option (fun o -> o#for_ident_expression) _x0 ;o#finish_ident_expression _x1 ;o#for_ident _x2 ;o#for_direction _x3 ;o#block _x4  end
|Continue ( _x0)  -> 
 begin o#label _x0  end
|Break -> ()
|Return ( _x0)  -> 
 begin o#expression _x0  end
|Int_switch ( _x0,_x1,_x2)  -> 
 begin o#expression _x0 ;o#list (fun o -> o#int_clause) _x1 ;o#option (fun o -> o#block) _x2  end
|String_switch ( _x0,_x1,_x2)  -> 
 begin o#expression _x0 ;o#list (fun o -> o#string_clause) _x1 ;o#option (fun o -> o#block) _x2  end
|Throw ( _x0)  -> 
 begin o#expression _x0  end
|Try ( _x0,_x1,_x2)  -> 
 begin o#block _x0 ;o#option (fun o -> fun ( _x0,_x1) -> begin o#exception_ident _x0;o#block _x1 end) _x1 ;o#option (fun o -> o#block) _x2  end
|Debugger -> ()  
method expression : expression -> unit = fun { expression_desc = _x0;comment = _x1} -> begin o#expression_desc _x0;o#option (fun o -> o#unknown) _x1 end  
method statement : statement -> unit = fun { statement_desc = _x0;comment = _x1} -> begin o#statement_desc _x0;o#option (fun o -> o#unknown) _x1 end  
method variable_declaration : variable_declaration -> unit = fun { ident = _x0;value = _x1;property = _x2;ident_info = _x3} -> begin o#ident _x0;o#option (fun o -> o#expression) _x1 end  
method string_clause : string_clause -> unit = fun ( _x0,_x1) -> begin o#case_clause _x1 end  
method int_clause : int_clause -> unit = fun ( _x0,_x1) -> begin o#case_clause _x1 end  
method case_clause : case_clause -> unit = fun { switch_body = _x0;should_break = _x1;comment = _x2} -> begin o#block _x0;o#option (fun o -> o#unknown) _x2 end  
method block : block -> unit = o#list (fun o -> o#statement)  
method program : program -> unit = fun { block = _x0;exports = _x1;export_set = _x2} -> begin o#block _x0 end  
method deps_program : deps_program -> unit = fun { program = _x0;modules = _x1;side_effect = _x2} -> begin o#program _x0;o#required_modules _x1;o#option (fun o -> o#unknown) _x2 end      
    end
    