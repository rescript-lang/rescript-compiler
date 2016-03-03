type exception_block = int * string * int 
val out_of_memory : exception_block              
val sys_error  : exception_block                                
val failure : exception_block                                  
val invalid_argument : exception_block                         
val end_of_file : exception_block                              
val division_by_zero : exception_block                         
val not_found : exception_block                                
val match_failure : exception_block                            
val stack_overflow : exception_block                           
val sys_blocked_io : exception_block                           
val assert_failure : exception_block                          
val undefined_recursive_module : exception_block              

val caml_set_oo_id : exception_block -> exception_block
val get_id : unit -> nativeint
 
