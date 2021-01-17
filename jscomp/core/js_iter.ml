
    open J  
    class virtual iter =
      object ((o : 'self_type))
        method unknown : 'a. 'a -> unit = ignore
        method string : string -> unit = ignore
        method option :
          'a. ('self_type -> 'a -> unit) -> 'a option -> unit =
          fun _f_a -> function | None -> () | Some _x ->  _f_a o _x 
        method list :
          'a. ('self_type -> 'a -> unit) -> 'a list -> unit =
          fun _f_a ->
            function
            | [] -> ()
            | _x :: _x_i1 -> _f_a o _x ;  o#list _f_a _x_i1 
        method int32 : int32 -> unit = ignore
        method int : int -> unit = ignore
        method bool : bool -> unit = ignore
    method label : label -> unit = o#string  
method binop : binop -> unit = o#unknown  
method int_op : int_op -> unit = o#unknown  
method kind : kind -> unit = o#unknown  
method property : property -> unit = o#unknown  
method number : number -> unit = o#unknown  
method mutable_flag : mutable_flag -> unit = o#unknown  
method ident_info : ident_info -> unit = o#unknown  
method exports : exports -> unit = o#unknown  
method tag_info : tag_info -> unit = o#unknown  
method required_modules : required_modules -> unit = o#list (fun o -> o#module_id)  
method property_name : property_name -> unit = o#unknown  
method ident : ident -> unit = o#unknown  
method module_id : module_id -> unit = fun { id = _x0;kind = _x1} -> begin o#ident _x0;o#unknown _x1 end  
method vident : vident -> unit = function 
| Id ( _x0)  -> 
 begin o#ident _x0  end
|Qualified ( _x0,_x1)  -> 
 begin o#module_id _x0 ;o#option (fun o -> o#string) _x1  end  
method exception_ident : exception_ident -> unit = o#ident  
method for_ident : for_ident -> unit = o#ident  
method for_direction : for_direction -> unit = o#unknown  
method property_map : property_map -> unit = o#list (fun o -> fun ( _x0,_x1) -> begin o#property_name _x0;o#expression _x1 end)  
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
 begin o#bool _x0  end
|Typeof ( _x0)  -> 
 begin o#expression _x0  end
|Js_not ( _x0)  -> 
 begin o#expression _x0  end
|Seq ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#expression _x1  end
|Cond ( _x0,_x1,_x2)  -> 
 begin o#expression _x0 ;o#expression _x1 ;o#expression _x2  end
|Bin ( _x0,_x1,_x2)  -> 
 begin o#binop _x0 ;o#expression _x1 ;o#expression _x2  end
|FlatCall ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#expression _x1  end
|Call ( _x0,_x1,_x2)  -> 
 begin o#expression _x0 ;o#list (fun o -> o#expression) _x1 ;o#unknown _x2  end
|String_index ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#expression _x1  end
|Array_index ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#expression _x1  end
|Static_index ( _x0,_x1,_x2)  -> 
 begin o#expression _x0 ;o#string _x1 ;o#option (fun o -> o#int32) _x2  end
|New ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#option (fun o -> o#list (fun o -> o#expression)) _x1  end
|Var ( _x0)  -> 
 begin o#vident _x0  end
|Fun ( _x0,_x1,_x2,_x3)  -> 
 begin o#bool _x0 ;o#list (fun o -> o#ident) _x1 ;o#block _x2 ;o#unknown _x3  end
|Str ( _x0,_x1)  -> 
 begin o#bool _x0 ;o#string _x1  end
|Unicode ( _x0)  -> 
 begin o#string _x0  end
|Raw_js_code ( _x0)  -> 
 begin o#unknown _x0  end
|Array ( _x0,_x1)  -> 
 begin o#list (fun o -> o#expression) _x0 ;o#mutable_flag _x1  end
|Optional_block ( _x0,_x1)  -> 
 begin o#expression _x0 ;o#bool _x1  end
|Caml_block ( _x0,_x1,_x2,_x3)  -> 
 begin o#list (fun o -> o#expression) _x0 ;o#mutable_flag _x1 ;o#expression _x2 ;o#tag_info _x3  end
|Caml_block_tag ( _x0)  -> 
 begin o#expression _x0  end
|Number ( _x0)  -> 
 begin o#number _x0  end
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
 begin o#option (fun o -> o#label) _x0 ;o#expression _x1 ;o#block _x2 ;o#unknown _x3  end
|ForRange ( _x0,_x1,_x2,_x3,_x4,_x5)  -> 
 begin o#option (fun o -> o#for_ident_expression) _x0 ;o#finish_ident_expression _x1 ;o#for_ident _x2 ;o#for_direction _x3 ;o#block _x4 ;o#unknown _x5  end
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
method expression : expression -> unit = fun { expression_desc = _x0;comment = _x1} -> begin o#expression_desc _x0;o#option (fun o -> o#string) _x1 end  
method statement : statement -> unit = fun { statement_desc = _x0;comment = _x1} -> begin o#statement_desc _x0;o#option (fun o -> o#string) _x1 end  
method variable_declaration : variable_declaration -> unit = fun { ident = _x0;value = _x1;property = _x2;ident_info = _x3} -> begin o#ident _x0;o#option (fun o -> o#expression) _x1;o#property _x2;o#ident_info _x3 end  
method string_clause : string_clause -> unit = fun ( _x0,_x1) -> begin o#string _x0;o#case_clause _x1 end  
method int_clause : int_clause -> unit = fun ( _x0,_x1) -> begin o#int _x0;o#case_clause _x1 end  
method case_clause : case_clause -> unit = fun { switch_body = _x0;should_break = _x1;comment = _x2} -> begin o#block _x0;o#bool _x1;o#option (fun o -> o#string) _x2 end  
method block : block -> unit = o#list (fun o -> o#statement)  
method program : program -> unit = fun { block = _x0;exports = _x1;export_set = _x2} -> begin o#block _x0;o#exports _x1;o#unknown _x2 end  
method deps_program : deps_program -> unit = fun { program = _x0;modules = _x1;side_effect = _x2} -> begin o#program _x0;o#required_modules _x1;o#option (fun o -> o#string) _x2 end      
    end
    