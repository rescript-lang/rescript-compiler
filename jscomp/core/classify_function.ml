

let classify (prog : string) : Js_raw_exp_info.t = 
  match Parser_flow.parse_expression 
    (Parser_env.init_env None prog) false with 
  | (_, Function {
    id = None;
    params = (_, {params});
    async = false;
    generator = false;
    predicate = None
  }) , [] -> 
    Js_function {arity = List.length params; arrow = false}
  | (_, ArrowFunction {
    id = None;
    params = (_, {params});
    async = false;
    generator = false;
    predicate = None
  }) , [] -> 
    Js_function
      {arity = List.length params; arrow = true} 
 |(_, Literal _), [] -> 
  Js_literal     
 | _ -> 
  Js_unknown
 | exception _ -> 
  Js_unknown
(* we can also analayze throw
  x.x pure access
 *)