

type (-'obj, +'a) meth_callback 
type (-'arg, + 'result) meth 

external (!)  : 'a Js.t -> 'a = "#unsafe_downgrade"
 
external unsafe_downgrade : 'a Js.t -> 'a = "#unsafe_downgrade"


external fn_method1 : ('a0 -> 'a1) -> (([`Arity_1 of ( 'a0 )], 'a1 ) meth_callback) = "#fn_method" "1"
external fn_method2 : ('a0 -> 'a1 -> 'a2) -> (([`Arity_2 of ( 'a0 * 'a1 )], 'a2 ) meth_callback) = "#fn_method" "2"
external fn_method3 : ('a0 -> 'a1 -> 'a2 -> 'a3) -> (([`Arity_3 of ( 'a0 * 'a1 * 'a2 )], 'a3 ) meth_callback) = "#fn_method" "3"
external fn_method4 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4) -> (([`Arity_4 of ( 'a0 * 'a1 * 'a2 * 'a3 )], 'a4 ) meth_callback) = "#fn_method" "4"
external fn_method5 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5) -> (([`Arity_5 of ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 )], 'a5 ) meth_callback) = "#fn_method" "5"
external fn_method6 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6) -> (([`Arity_6 of ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 )], 'a6 ) meth_callback) = "#fn_method" "6"
external fn_method7 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7) -> (([`Arity_7 of ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 )], 'a7 ) meth_callback) = "#fn_method" "7"
external fn_method8 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8) -> (([`Arity_8 of ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 )], 'a8 ) meth_callback) = "#fn_method" "8"
external fn_method9 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9) -> (([`Arity_9 of ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 )], 'a9 ) meth_callback) = "#fn_method" "9"
external fn_method10 : ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 -> 'a10) -> (([`Arity_10 of ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 )], 'a10 ) meth_callback) = "#fn_method" "10"


external method_run0 : (([`Arity_0], 'a0) meth) -> 'a0 = "#method_run" "0"
external method_run1 : (([`Arity_1 of ( 'a0 )], 'a1) meth) -> ('a0 -> 'a1 ) = "#method_run" "1"
external method_run2 : (([`Arity_2 of ( 'a0 * 'a1 )], 'a2) meth) -> ('a0 -> 'a1 -> 'a2 ) = "#method_run" "2"
external method_run3 : (([`Arity_3 of ( 'a0 * 'a1 * 'a2 )], 'a3) meth) -> ('a0 -> 'a1 -> 'a2 -> 'a3 ) = "#method_run" "3"
external method_run4 : (([`Arity_4 of ( 'a0 * 'a1 * 'a2 * 'a3 )], 'a4) meth) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 ) = "#method_run" "4"
external method_run5 : (([`Arity_5 of ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 )], 'a5) meth) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 ) = "#method_run" "5"
external method_run6 : (([`Arity_6 of ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 )], 'a6) meth) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 ) = "#method_run" "6"
external method_run7 : (([`Arity_7 of ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 )], 'a7) meth) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 ) = "#method_run" "7"
external method_run8 : (([`Arity_8 of ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 )], 'a8) meth) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 ) = "#method_run" "8"
external method_run9 : (([`Arity_9 of ( 'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 )], 'a9) meth) -> ('a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 ) = "#method_run" "9"