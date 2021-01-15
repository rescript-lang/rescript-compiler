
    open J  
    class virtual fold =
      object ((o : 'self_type))
        method unknown : 'a. 'a -> 'self_type = fun _ -> o
        method string : string -> 'self_type = fun _ -> o
        method option :
          'a. ('self_type -> 'a -> 'self_type) -> 'a option -> 'self_type =
          fun _f_a -> function | None -> o | Some _x -> let o = _f_a o _x in o
        method list :
          'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
          fun _f_a ->
            function
            | [] -> o
            | _x :: _x_i1 -> let o = _f_a o _x in let o = o#list _f_a _x_i1 in o
        method int32 : int32 -> 'self_type = fun _ -> o
        method int : int -> 'self_type = fun _ -> o 
        method bool : bool -> 'self_type = fun _ -> o
    method label : label -> 'self_type = o#string  
method binop : binop -> 'self_type = o#unknown  
method int_op : int_op -> 'self_type = o#unknown  
method kind : kind -> 'self_type = o#unknown  
method property : property -> 'self_type = o#unknown  
method number : number -> 'self_type = o#unknown  
method mutable_flag : mutable_flag -> 'self_type = o#unknown  
method ident_info : ident_info -> 'self_type = o#unknown  
method exports : exports -> 'self_type = o#unknown  
method tag_info : tag_info -> 'self_type = o#unknown  
method required_modules : required_modules -> 'self_type = o#list (fun o -> o#module_id)  
method property_name : property_name -> 'self_type = o#unknown  
method jsint : jsint -> 'self_type = o#int32  
method ident : ident -> 'self_type = o#unknown  
method module_id : module_id -> 'self_type = fun { id = _x0;kind = _x1} -> let o = o#ident _x0 in
let o = o#unknown _x1 in o  
method vident : vident -> 'self_type = function 
| Id ( _x0)  -> 
let o = o#ident _x0 in
 o
|Qualified ( _x0,_x1)  -> 
let o = o#module_id _x0 in
let o = o#option (fun o -> o#string) _x1 in
 o  
method exception_ident : exception_ident -> 'self_type = o#ident  
method for_ident : for_ident -> 'self_type = o#ident  
method for_direction : for_direction -> 'self_type = o#unknown  
method property_map : property_map -> 'self_type = o#list (fun o -> fun ( _x0,_x1) -> let o = o#property_name _x0 in let o = o#expression _x1 in o)  
method length_object : length_object -> 'self_type = o#unknown  
method expression_desc : expression_desc -> 'self_type = function 
| Length ( _x0,_x1)  -> 
let o = o#expression _x0 in
let o = o#length_object _x1 in
 o
|Char_of_int ( _x0)  -> 
let o = o#expression _x0 in
 o
|Char_to_int ( _x0)  -> 
let o = o#expression _x0 in
 o
|Is_null_or_undefined ( _x0)  -> 
let o = o#expression _x0 in
 o
|String_append ( _x0,_x1)  -> 
let o = o#expression _x0 in
let o = o#expression _x1 in
 o
|Bool ( _x0)  -> 
let o = o#bool _x0 in
 o
|Typeof ( _x0)  -> 
let o = o#expression _x0 in
 o
|Js_not ( _x0)  -> 
let o = o#expression _x0 in
 o
|Seq ( _x0,_x1)  -> 
let o = o#expression _x0 in
let o = o#expression _x1 in
 o
|Cond ( _x0,_x1,_x2)  -> 
let o = o#expression _x0 in
let o = o#expression _x1 in
let o = o#expression _x2 in
 o
|Bin ( _x0,_x1,_x2)  -> 
let o = o#binop _x0 in
let o = o#expression _x1 in
let o = o#expression _x2 in
 o
|FlatCall ( _x0,_x1)  -> 
let o = o#expression _x0 in
let o = o#expression _x1 in
 o
|Call ( _x0,_x1,_x2)  -> 
let o = o#expression _x0 in
let o = o#list (fun o -> o#expression) _x1 in
let o = o#unknown _x2 in
 o
|String_index ( _x0,_x1)  -> 
let o = o#expression _x0 in
let o = o#expression _x1 in
 o
|Array_index ( _x0,_x1)  -> 
let o = o#expression _x0 in
let o = o#expression _x1 in
 o
|Static_index ( _x0,_x1,_x2)  -> 
let o = o#expression _x0 in
let o = o#string _x1 in
let o = o#option (fun o -> o#int32) _x2 in
 o
|New ( _x0,_x1)  -> 
let o = o#expression _x0 in
let o = o#option (fun o -> o#list (fun o -> o#expression)) _x1 in
 o
|Var ( _x0)  -> 
let o = o#vident _x0 in
 o
|Fun ( _x0,_x1,_x2,_x3)  -> 
let o = o#bool _x0 in
let o = o#list (fun o -> o#ident) _x1 in
let o = o#block _x2 in
let o = o#unknown _x3 in
 o
|Str ( _x0,_x1)  -> 
let o = o#bool _x0 in
let o = o#string _x1 in
 o
|Unicode ( _x0)  -> 
let o = o#string _x0 in
 o
|Raw_js_code ( _x0)  -> 
let o = o#unknown _x0 in
 o
|Array ( _x0,_x1)  -> 
let o = o#list (fun o -> o#expression) _x0 in
let o = o#mutable_flag _x1 in
 o
|Optional_block ( _x0,_x1)  -> 
let o = o#expression _x0 in
let o = o#bool _x1 in
 o
|Caml_block ( _x0,_x1,_x2,_x3)  -> 
let o = o#list (fun o -> o#expression) _x0 in
let o = o#mutable_flag _x1 in
let o = o#expression _x2 in
let o = o#tag_info _x3 in
 o
|Caml_block_tag ( _x0)  -> 
let o = o#expression _x0 in
 o
|Number ( _x0)  -> 
let o = o#number _x0 in
 o
|Object ( _x0)  -> 
let o = o#property_map _x0 in
 o
|Undefined -> o
|Null -> o  
method for_ident_expression : for_ident_expression -> 'self_type = o#expression  
method finish_ident_expression : finish_ident_expression -> 'self_type = o#expression  
method statement_desc : statement_desc -> 'self_type = function 
| Block ( _x0)  -> 
let o = o#block _x0 in
 o
|Variable ( _x0)  -> 
let o = o#variable_declaration _x0 in
 o
|Exp ( _x0)  -> 
let o = o#expression _x0 in
 o
|If ( _x0,_x1,_x2)  -> 
let o = o#expression _x0 in
let o = o#block _x1 in
let o = o#block _x2 in
 o
|While ( _x0,_x1,_x2,_x3)  -> 
let o = o#option (fun o -> o#label) _x0 in
let o = o#expression _x1 in
let o = o#block _x2 in
let o = o#unknown _x3 in
 o
|ForRange ( _x0,_x1,_x2,_x3,_x4,_x5)  -> 
let o = o#option (fun o -> o#for_ident_expression) _x0 in
let o = o#finish_ident_expression _x1 in
let o = o#for_ident _x2 in
let o = o#for_direction _x3 in
let o = o#block _x4 in
let o = o#unknown _x5 in
 o
|Continue ( _x0)  -> 
let o = o#label _x0 in
 o
|Break -> o
|Return ( _x0)  -> 
let o = o#expression _x0 in
 o
|Int_switch ( _x0,_x1,_x2)  -> 
let o = o#expression _x0 in
let o = o#list (fun o -> o#int_clause) _x1 in
let o = o#option (fun o -> o#block) _x2 in
 o
|String_switch ( _x0,_x1,_x2)  -> 
let o = o#expression _x0 in
let o = o#list (fun o -> o#string_clause) _x1 in
let o = o#option (fun o -> o#block) _x2 in
 o
|Throw ( _x0)  -> 
let o = o#expression _x0 in
 o
|Try ( _x0,_x1,_x2)  -> 
let o = o#block _x0 in
let o = o#option (fun o -> fun ( _x0,_x1) -> let o = o#exception_ident _x0 in let o = o#block _x1 in o) _x1 in
let o = o#option (fun o -> o#block) _x2 in
 o
|Debugger -> o  
method expression : expression -> 'self_type = fun { expression_desc = _x0;comment = _x1} -> let o = o#expression_desc _x0 in
let o = o#option (fun o -> o#string) _x1 in o  
method statement : statement -> 'self_type = fun { statement_desc = _x0;comment = _x1} -> let o = o#statement_desc _x0 in
let o = o#option (fun o -> o#string) _x1 in o  
method variable_declaration : variable_declaration -> 'self_type = fun { ident = _x0;value = _x1;property = _x2;ident_info = _x3} -> let o = o#ident _x0 in
let o = o#option (fun o -> o#expression) _x1 in
let o = o#property _x2 in
let o = o#ident_info _x3 in o  
method string_clause : string_clause -> 'self_type = fun ( _x0,_x1) -> let o = o#string _x0 in let o = o#case_clause _x1 in o  
method int_clause : int_clause -> 'self_type = fun ( _x0,_x1) -> let o = o#int _x0 in let o = o#case_clause _x1 in o  
method case_clause : case_clause -> 'self_type = fun { switch_body = _x0;should_break = _x1;comment = _x2} -> let o = o#block _x0 in
let o = o#bool _x1 in
let o = o#option (fun o -> o#string) _x2 in o  
method block : block -> 'self_type = o#list (fun o -> o#statement)  
method program : program -> 'self_type = fun { block = _x0;exports = _x1;export_set = _x2} -> let o = o#block _x0 in
let o = o#exports _x1 in
let o = o#unknown _x2 in o  
method deps_program : deps_program -> 'self_type = fun { program = _x0;modules = _x1;side_effect = _x2} -> let o = o#program _x0 in
let o = o#required_modules _x1 in
let o = o#option (fun o -> o#string) _x2 in o      
    end
    