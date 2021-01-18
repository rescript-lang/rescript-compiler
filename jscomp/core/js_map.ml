
open J
let unknown : 'a. 'a -> 'a = fun x -> x 
class map = object
((_self : 'self_type))
method option :
  'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a option -> 'a_out option =
  fun _f_a ->
    function | None -> None | Some _x -> let _x = _f_a _self _x in Some _x
method list :
  'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a list -> 'a_out list =
  fun _f_a ->
    function
    | [] -> []
    | _x :: _x_i1 ->
        let _x = _f_a _self _x in
        let _x_i1 = _self#list _f_a _x_i1 in _x :: _x_i1
method label : label -> label = unknown  
method required_modules : required_modules -> required_modules = _self#list (fun _self -> _self#module_id)  
method ident : ident -> ident = unknown  
method module_id : module_id -> module_id = fun { id = _x0;kind = _x1} -> let _x0 = _self#ident _x0 in  {id = _x0;kind = _x1}  
method vident : vident -> vident = function 
| Id ( _x0)  -> 
let _x0 = _self#ident _x0 in 
Id ( _x0) 
|Qualified ( _x0,_x1)  -> 
let _x0 = _self#module_id _x0 in 
let _x1 = _self#option (fun _self -> unknown) _x1 in 
Qualified ( _x0,_x1)   
method exception_ident : exception_ident -> exception_ident = _self#ident  
method for_ident : for_ident -> for_ident = _self#ident  
method for_direction : for_direction -> for_direction = unknown  
method property_map : property_map -> property_map = _self#list (fun _self -> fun ( _x0,_x1) -> let _x1 = _self#expression _x1 in  _x0,_x1)  
method length_object : length_object -> length_object = unknown  
method expression_desc : expression_desc -> expression_desc = function 
| Length ( _x0,_x1)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#length_object _x1 in 
Length ( _x0,_x1) 
|Char_of_int ( _x0)  -> 
let _x0 = _self#expression _x0 in 
Char_of_int ( _x0) 
|Char_to_int ( _x0)  -> 
let _x0 = _self#expression _x0 in 
Char_to_int ( _x0) 
|Is_null_or_undefined ( _x0)  -> 
let _x0 = _self#expression _x0 in 
Is_null_or_undefined ( _x0) 
|String_append ( _x0,_x1)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#expression _x1 in 
String_append ( _x0,_x1) 
|Bool _ as v -> v 
|Typeof ( _x0)  -> 
let _x0 = _self#expression _x0 in 
Typeof ( _x0) 
|Js_not ( _x0)  -> 
let _x0 = _self#expression _x0 in 
Js_not ( _x0) 
|Seq ( _x0,_x1)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#expression _x1 in 
Seq ( _x0,_x1) 
|Cond ( _x0,_x1,_x2)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#expression _x1 in 
let _x2 = _self#expression _x2 in 
Cond ( _x0,_x1,_x2) 
|Bin ( _x0,_x1,_x2)  -> 
let _x1 = _self#expression _x1 in 
let _x2 = _self#expression _x2 in 
Bin ( _x0,_x1,_x2) 
|FlatCall ( _x0,_x1)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#expression _x1 in 
FlatCall ( _x0,_x1) 
|Call ( _x0,_x1,_x2)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#list (fun _self -> _self#expression) _x1 in 
Call ( _x0,_x1,_x2) 
|String_index ( _x0,_x1)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#expression _x1 in 
String_index ( _x0,_x1) 
|Array_index ( _x0,_x1)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#expression _x1 in 
Array_index ( _x0,_x1) 
|Static_index ( _x0,_x1,_x2)  -> 
let _x0 = _self#expression _x0 in 
let _x2 = _self#option (fun _self -> unknown) _x2 in 
Static_index ( _x0,_x1,_x2) 
|New ( _x0,_x1)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#option (fun _self -> _self#list (fun _self -> _self#expression)) _x1 in 
New ( _x0,_x1) 
|Var ( _x0)  -> 
let _x0 = _self#vident _x0 in 
Var ( _x0) 
|Fun ( _x0,_x1,_x2,_x3)  -> 
let _x1 = _self#list (fun _self -> _self#ident) _x1 in 
let _x2 = _self#block _x2 in 
Fun ( _x0,_x1,_x2,_x3) 
|Str _ as v -> v 
|Unicode _ as v -> v 
|Raw_js_code _ as v -> v 
|Array ( _x0,_x1)  -> 
let _x0 = _self#list (fun _self -> _self#expression) _x0 in 
Array ( _x0,_x1) 
|Optional_block ( _x0,_x1)  -> 
let _x0 = _self#expression _x0 in 
Optional_block ( _x0,_x1) 
|Caml_block ( _x0,_x1,_x2,_x3)  -> 
let _x0 = _self#list (fun _self -> _self#expression) _x0 in 
let _x2 = _self#expression _x2 in 
Caml_block ( _x0,_x1,_x2,_x3) 
|Caml_block_tag ( _x0)  -> 
let _x0 = _self#expression _x0 in 
Caml_block_tag ( _x0) 
|Number _ as v -> v 
|Object ( _x0)  -> 
let _x0 = _self#property_map _x0 in 
Object ( _x0) 
|Undefined as v -> v
|Null as v -> v  
method for_ident_expression : for_ident_expression -> for_ident_expression = _self#expression  
method finish_ident_expression : finish_ident_expression -> finish_ident_expression = _self#expression  
method statement_desc : statement_desc -> statement_desc = function 
| Block ( _x0)  -> 
let _x0 = _self#block _x0 in 
Block ( _x0) 
|Variable ( _x0)  -> 
let _x0 = _self#variable_declaration _x0 in 
Variable ( _x0) 
|Exp ( _x0)  -> 
let _x0 = _self#expression _x0 in 
Exp ( _x0) 
|If ( _x0,_x1,_x2)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#block _x1 in 
let _x2 = _self#block _x2 in 
If ( _x0,_x1,_x2) 
|While ( _x0,_x1,_x2,_x3)  -> 
let _x0 = _self#option (fun _self -> _self#label) _x0 in 
let _x1 = _self#expression _x1 in 
let _x2 = _self#block _x2 in 
While ( _x0,_x1,_x2,_x3) 
|ForRange ( _x0,_x1,_x2,_x3,_x4,_x5)  -> 
let _x0 = _self#option (fun _self -> _self#for_ident_expression) _x0 in 
let _x1 = _self#finish_ident_expression _x1 in 
let _x2 = _self#for_ident _x2 in 
let _x3 = _self#for_direction _x3 in 
let _x4 = _self#block _x4 in 
ForRange ( _x0,_x1,_x2,_x3,_x4,_x5) 
|Continue ( _x0)  -> 
let _x0 = _self#label _x0 in 
Continue ( _x0) 
|Break as v -> v
|Return ( _x0)  -> 
let _x0 = _self#expression _x0 in 
Return ( _x0) 
|Int_switch ( _x0,_x1,_x2)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#list (fun _self -> _self#int_clause) _x1 in 
let _x2 = _self#option (fun _self -> _self#block) _x2 in 
Int_switch ( _x0,_x1,_x2) 
|String_switch ( _x0,_x1,_x2)  -> 
let _x0 = _self#expression _x0 in 
let _x1 = _self#list (fun _self -> _self#string_clause) _x1 in 
let _x2 = _self#option (fun _self -> _self#block) _x2 in 
String_switch ( _x0,_x1,_x2) 
|Throw ( _x0)  -> 
let _x0 = _self#expression _x0 in 
Throw ( _x0) 
|Try ( _x0,_x1,_x2)  -> 
let _x0 = _self#block _x0 in 
let _x1 = _self#option (fun _self -> fun ( _x0,_x1) -> let _x0 = _self#exception_ident _x0 in  let _x1 = _self#block _x1 in  _x0,_x1) _x1 in 
let _x2 = _self#option (fun _self -> _self#block) _x2 in 
Try ( _x0,_x1,_x2) 
|Debugger as v -> v  
method expression : expression -> expression = fun { expression_desc = _x0;comment = _x1} -> let _x0 = _self#expression_desc _x0 in 
let _x1 = _self#option (fun _self -> unknown) _x1 in  {expression_desc = _x0;comment = _x1}  
method statement : statement -> statement = fun { statement_desc = _x0;comment = _x1} -> let _x0 = _self#statement_desc _x0 in 
let _x1 = _self#option (fun _self -> unknown) _x1 in  {statement_desc = _x0;comment = _x1}  
method variable_declaration : variable_declaration -> variable_declaration = fun { ident = _x0;value = _x1;property = _x2;ident_info = _x3} -> let _x0 = _self#ident _x0 in 
let _x1 = _self#option (fun _self -> _self#expression) _x1 in  {ident = _x0;value = _x1;property = _x2;ident_info = _x3}  
method string_clause : string_clause -> string_clause = fun ( _x0,_x1) -> let _x1 = _self#case_clause _x1 in  _x0,_x1  
method int_clause : int_clause -> int_clause = fun ( _x0,_x1) -> let _x1 = _self#case_clause _x1 in  _x0,_x1  
method case_clause : case_clause -> case_clause = fun { switch_body = _x0;should_break = _x1;comment = _x2} -> let _x0 = _self#block _x0 in 
let _x2 = _self#option (fun _self -> unknown) _x2 in  {switch_body = _x0;should_break = _x1;comment = _x2}  
method block : block -> block = _self#list (fun _self -> _self#statement)  
method program : program -> program = fun { block = _x0;exports = _x1;export_set = _x2} -> let _x0 = _self#block _x0 in  {block = _x0;exports = _x1;export_set = _x2}  
method deps_program : deps_program -> deps_program = fun { program = _x0;modules = _x1;side_effect = _x2} -> let _x0 = _self#program _x0 in 
let _x1 = _self#required_modules _x1 in 
let _x2 = _self#option (fun _self -> unknown) _x2 in  {program = _x0;modules = _x1;side_effect = _x2}  
end
