

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
    Function {arity = List.length params; arrow = false}
  | (_, ArrowFunction {
    id = None;
    params = (_, {params});
    async = false;
    generator = false;
    predicate = None
  }) , [] -> 
    Function
      {arity = List.length params; arrow = true} 
 | _ -> 
  Unknown
 | exception _ -> 
  Unknown
(* we can also analayze throw
  x.x pure access
 *)