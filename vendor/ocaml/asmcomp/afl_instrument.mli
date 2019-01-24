(* Instrumentation for afl-fuzz *)

val instrument_function : Cmm.expression -> Cmm.expression
val instrument_initialiser : Cmm.expression -> Cmm.expression
