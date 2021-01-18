
open J
let unknown : 'a. 'a -> 'a = fun x -> x 
class map = object
((o : 'self_type))
method string : string -> string = fun x -> x
method option :
  'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a option -> 'a_out option =
  fun _f_a ->
    function | None -> None | Some _x -> let _x = _f_a o _x in Some _x
method list :
  'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a list -> 'a_out list =
  fun _f_a ->
    function
    | [] -> []
    | _x :: _x_i1 ->
        let _x = _f_a o _x in
        let _x_i1 = o#list _f_a _x_i1 in _x :: _x_i1
method int32 : int32 -> int32 = fun x -> x  
method int : int -> int = fun x -> x 
method bool : bool -> bool = fun x -> x
method label : label -> label = o#string  
method required_modules : required_modules -> required_modules = o#list (fun o -> o#module_id)  
method ident : ident -> ident = unknown  
method module_id : module_id -> module_id = fun { id = _x0;kind = _x1} -> let _x0 = o#ident _x0 in
let _x1 = unknown _x1 in {id = _x0;kind = _x1}  
method vident : vident -> vident = function 
| Id ( _x0)  -> 
let _x0 = o#ident _x0 in
Id ( _x0) 
|Qualified ( _x0,_x1)  -> 
let _x0 = o#module_id _x0 in
let _x1 = o#option (fun o -> o#string) _x1 in
Qualified ( _x0,_x1)   
method exception_ident : exception_ident -> exception_ident = o#ident  
method for_ident : for_ident -> for_ident = o#ident  
method for_direction : for_direction -> for_direction = unknown  
method property_map : property_map -> property_map = o#list (fun o -> fun ( _x0,_x1) -> let _x0 = unknown _x0 in let _x1 = o#expression _x1 in _x0,_x1)  
method length_object : length_object -> length_object = unknown  
method expression_desc : expression_desc -> expression_desc = function 
| Length ( _x0,_x1)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#length_object _x1 in
Length ( _x0,_x1) 
|Char_of_int ( _x0)  -> 
let _x0 = o#expression _x0 in
Char_of_int ( _x0) 
|Char_to_int ( _x0)  -> 
let _x0 = o#expression _x0 in
Char_to_int ( _x0) 
|Is_null_or_undefined ( _x0)  -> 
let _x0 = o#expression _x0 in
Is_null_or_undefined ( _x0) 
|String_append ( _x0,_x1)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#expression _x1 in
String_append ( _x0,_x1) 
|Bool ( _x0)  -> 
let _x0 = o#bool _x0 in
Bool ( _x0) 
|Typeof ( _x0)  -> 
let _x0 = o#expression _x0 in
Typeof ( _x0) 
|Js_not ( _x0)  -> 
let _x0 = o#expression _x0 in
Js_not ( _x0) 
|Seq ( _x0,_x1)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#expression _x1 in
Seq ( _x0,_x1) 
|Cond ( _x0,_x1,_x2)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#expression _x1 in
let _x2 = o#expression _x2 in
Cond ( _x0,_x1,_x2) 
|Bin ( _x0,_x1,_x2)  -> 
let _x0 = unknown _x0 in
let _x1 = o#expression _x1 in
let _x2 = o#expression _x2 in
Bin ( _x0,_x1,_x2) 
|FlatCall ( _x0,_x1)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#expression _x1 in
FlatCall ( _x0,_x1) 
|Call ( _x0,_x1,_x2)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#list (fun o -> o#expression) _x1 in
let _x2 = unknown _x2 in
Call ( _x0,_x1,_x2) 
|String_index ( _x0,_x1)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#expression _x1 in
String_index ( _x0,_x1) 
|Array_index ( _x0,_x1)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#expression _x1 in
Array_index ( _x0,_x1) 
|Static_index ( _x0,_x1,_x2)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#string _x1 in
let _x2 = o#option (fun o -> o#int32) _x2 in
Static_index ( _x0,_x1,_x2) 
|New ( _x0,_x1)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#option (fun o -> o#list (fun o -> o#expression)) _x1 in
New ( _x0,_x1) 
|Var ( _x0)  -> 
let _x0 = o#vident _x0 in
Var ( _x0) 
|Fun ( _x0,_x1,_x2,_x3)  -> 
let _x0 = o#bool _x0 in
let _x1 = o#list (fun o -> o#ident) _x1 in
let _x2 = o#block _x2 in
let _x3 = unknown _x3 in
Fun ( _x0,_x1,_x2,_x3) 
|Str ( _x0,_x1)  -> 
let _x0 = o#bool _x0 in
let _x1 = o#string _x1 in
Str ( _x0,_x1) 
|Unicode ( _x0)  -> 
let _x0 = o#string _x0 in
Unicode ( _x0) 
|Raw_js_code ( _x0)  -> 
let _x0 = unknown _x0 in
Raw_js_code ( _x0) 
|Array ( _x0,_x1)  -> 
let _x0 = o#list (fun o -> o#expression) _x0 in
let _x1 = unknown _x1 in
Array ( _x0,_x1) 
|Optional_block ( _x0,_x1)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#bool _x1 in
Optional_block ( _x0,_x1) 
|Caml_block ( _x0,_x1,_x2,_x3)  -> 
let _x0 = o#list (fun o -> o#expression) _x0 in
let _x1 = unknown _x1 in
let _x2 = o#expression _x2 in
let _x3 = unknown _x3 in
Caml_block ( _x0,_x1,_x2,_x3) 
|Caml_block_tag ( _x0)  -> 
let _x0 = o#expression _x0 in
Caml_block_tag ( _x0) 
|Number ( _x0)  -> 
let _x0 = unknown _x0 in
Number ( _x0) 
|Object ( _x0)  -> 
let _x0 = o#property_map _x0 in
Object ( _x0) 
|Undefined -> Undefined
|Null -> Null  
method for_ident_expression : for_ident_expression -> for_ident_expression = o#expression  
method finish_ident_expression : finish_ident_expression -> finish_ident_expression = o#expression  
method statement_desc : statement_desc -> statement_desc = function 
| Block ( _x0)  -> 
let _x0 = o#block _x0 in
Block ( _x0) 
|Variable ( _x0)  -> 
let _x0 = o#variable_declaration _x0 in
Variable ( _x0) 
|Exp ( _x0)  -> 
let _x0 = o#expression _x0 in
Exp ( _x0) 
|If ( _x0,_x1,_x2)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#block _x1 in
let _x2 = o#block _x2 in
If ( _x0,_x1,_x2) 
|While ( _x0,_x1,_x2,_x3)  -> 
let _x0 = o#option (fun o -> o#label) _x0 in
let _x1 = o#expression _x1 in
let _x2 = o#block _x2 in
let _x3 = unknown _x3 in
While ( _x0,_x1,_x2,_x3) 
|ForRange ( _x0,_x1,_x2,_x3,_x4,_x5)  -> 
let _x0 = o#option (fun o -> o#for_ident_expression) _x0 in
let _x1 = o#finish_ident_expression _x1 in
let _x2 = o#for_ident _x2 in
let _x3 = o#for_direction _x3 in
let _x4 = o#block _x4 in
let _x5 = unknown _x5 in
ForRange ( _x0,_x1,_x2,_x3,_x4,_x5) 
|Continue ( _x0)  -> 
let _x0 = o#label _x0 in
Continue ( _x0) 
|Break -> Break
|Return ( _x0)  -> 
let _x0 = o#expression _x0 in
Return ( _x0) 
|Int_switch ( _x0,_x1,_x2)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#list (fun o -> o#int_clause) _x1 in
let _x2 = o#option (fun o -> o#block) _x2 in
Int_switch ( _x0,_x1,_x2) 
|String_switch ( _x0,_x1,_x2)  -> 
let _x0 = o#expression _x0 in
let _x1 = o#list (fun o -> o#string_clause) _x1 in
let _x2 = o#option (fun o -> o#block) _x2 in
String_switch ( _x0,_x1,_x2) 
|Throw ( _x0)  -> 
let _x0 = o#expression _x0 in
Throw ( _x0) 
|Try ( _x0,_x1,_x2)  -> 
let _x0 = o#block _x0 in
let _x1 = o#option (fun o -> fun ( _x0,_x1) -> let _x0 = o#exception_ident _x0 in let _x1 = o#block _x1 in _x0,_x1) _x1 in
let _x2 = o#option (fun o -> o#block) _x2 in
Try ( _x0,_x1,_x2) 
|Debugger -> Debugger  
method expression : expression -> expression = fun { expression_desc = _x0;comment = _x1} -> let _x0 = o#expression_desc _x0 in
let _x1 = o#option (fun o -> o#string) _x1 in {expression_desc = _x0;comment = _x1}  
method statement : statement -> statement = fun { statement_desc = _x0;comment = _x1} -> let _x0 = o#statement_desc _x0 in
let _x1 = o#option (fun o -> o#string) _x1 in {statement_desc = _x0;comment = _x1}  
method variable_declaration : variable_declaration -> variable_declaration = fun { ident = _x0;value = _x1;property = _x2;ident_info = _x3} -> let _x0 = o#ident _x0 in
let _x1 = o#option (fun o -> o#expression) _x1 in
let _x2 = unknown _x2 in
let _x3 = unknown _x3 in {ident = _x0;value = _x1;property = _x2;ident_info = _x3}  
method string_clause : string_clause -> string_clause = fun ( _x0,_x1) -> let _x0 = o#string _x0 in let _x1 = o#case_clause _x1 in _x0,_x1  
method int_clause : int_clause -> int_clause = fun ( _x0,_x1) -> let _x0 = o#int _x0 in let _x1 = o#case_clause _x1 in _x0,_x1  
method case_clause : case_clause -> case_clause = fun { switch_body = _x0;should_break = _x1;comment = _x2} -> let _x0 = o#block _x0 in
let _x1 = o#bool _x1 in
let _x2 = o#option (fun o -> o#string) _x2 in {switch_body = _x0;should_break = _x1;comment = _x2}  
method block : block -> block = o#list (fun o -> o#statement)  
method program : program -> program = fun { block = _x0;exports = _x1;export_set = _x2} -> let _x0 = o#block _x0 in
let _x1 = unknown _x1 in
let _x2 = unknown _x2 in {block = _x0;exports = _x1;export_set = _x2}  
method deps_program : deps_program -> deps_program = fun { program = _x0;modules = _x1;side_effect = _x2} -> let _x0 = o#program _x0 in
let _x1 = o#required_modules _x1 in
let _x2 = o#option (fun o -> o#string) _x2 in {program = _x0;modules = _x1;side_effect = _x2}  
end
