type env = (string, Sexp_lexer.t) Hashtbl.t

let rec print fmt (x : Sexp_lexer.t) =
  match x with 
  | Atom v -> 
    Format.pp_print_string fmt v
  | Lit s -> 
    Format.fprintf fmt "%S" s (* TODO: string escaping*)
  | List vs -> 
    Format.fprintf fmt  "@[(@ %a@ )@]" 
      (Format.pp_print_list ~pp_sep:(Format.pp_print_space) print ) vs 
  | Data vs
    -> 
    Format.fprintf fmt "@['(@ %a@ )@]" 
      (Format.pp_print_list ~pp_sep:(Format.pp_print_space) print) vs 

let print_env fmt  env = 
  Hashtbl.iter (fun k v -> 
      Format.fprintf fmt
        "@[%s@ ->@ %a@]@."
        k print v 
    ) env 


let nil : Sexp_lexer.t = List []  

exception Unbound_value of string 

let rec eval env (x : Sexp_lexer.t) : Sexp_lexer.t =
  match x with 
  | Atom x ->
    begin match Hashtbl.find env x  with
    | exception Not_found 
      -> raise (Unbound_value x)
    | x -> x 
    end
  | Lit _ -> x 
  | Data xs -> List xs
  | List [] -> nil
  | List (command::rest) -> 
    begin match command with 
    | Atom "setq" 
      -> let rec loop last_value rest =
           match rest with 
           | Sexp_lexer.Atom x :: v :: rest -> 
             let xvalue = eval env v in 
             Hashtbl.add env x xvalue ; 
             loop xvalue rest 
           | [Atom x] 
             ->  Hashtbl.add env x nil ; nil 
           | [] -> last_value
           | (Lit _ | Data _ | List _ ) :: _  
             -> assert false  in 
      loop nil rest
    | _ -> assert false
    end

let eval_file s = 
  let sexps = Sexp_lexer.from_file s in 
  let env = Hashtbl.create 64 in 
  List.iter (fun x -> ignore (eval env x )) sexps ; 
  env 

let eval_string s = 
  let sexps = Sexp_lexer.token (Lexing.from_string s) in 
  let env = Hashtbl.create 64 in 
  List.iter (fun x -> ignore (eval env x )) sexps ; 
  env 
