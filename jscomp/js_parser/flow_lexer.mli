



val token:
  Lex_env.t -> Lex_env.t * Lex_result.t

val type_token:  
  Lex_env.t -> Lex_env.t * Lex_result.t

val jsx_tag : 
  Lex_env.t -> Lex_env.t * Lex_result.t

val jsx_child :  
  Lex_env.t -> Lex_env.t * Lex_result.t

val template_tail :
  Lex_env.t -> Lex_env.t * Lex_result.t

val regexp:
  Lex_env.t -> Lex_env.t * Lex_result.t