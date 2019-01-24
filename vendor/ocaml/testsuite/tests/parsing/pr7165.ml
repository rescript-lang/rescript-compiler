(* this is a lexer directive with an out-of-bound integer;
   it should result in a lexing error instead of an
   uncaught exception as in PR#7165 *)
#9342101923012312312
