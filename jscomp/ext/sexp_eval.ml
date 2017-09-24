type env =  Sexp_lexer.t String_hashtbl.t

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
  String_hashtbl.iter (fun k v -> 
      Format.fprintf fmt
        "@[%s@ ->@ %a@]@."
        k print v 
    ) env 


let nil : Sexp_lexer.t = List []  

exception Unbound_value of string 

let rec eval env (x : Sexp_lexer.t) : Sexp_lexer.t =
  match x with 
  | Atom x ->
    begin match String_hashtbl.find_opt env x  with
    | None
      -> raise (Unbound_value x)
    | Some x -> x 
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
             String_hashtbl.add env x xvalue ; 
             loop xvalue rest 
           | [Atom x] 
             ->  String_hashtbl.add env x nil ; nil 
           | [] -> last_value
           | (Lit _ | Data _ | List _ ) :: _  
             -> assert false  in 
      loop nil rest
    | _ -> assert false
    end

let eval_file s = 
  let sexps = Sexp_lexer.from_file s in 
  let env : Sexp_lexer.t String_hashtbl.t= String_hashtbl.create 64 in 
  List.iter (fun x -> ignore (eval env x )) sexps ; 
  env 

let eval_string s = 
  let sexps = Sexp_lexer.token (Lexing.from_string s) in 
  let env : Sexp_lexer.t String_hashtbl.t = String_hashtbl.create 64 in 
  List.iter (fun x -> ignore (eval env x )) sexps ; 
  env 

type ty = 
  | Any
  | String 
  | List of ty

exception Expect of string * ty

let error (x,ty) = raise (Expect (x,ty) )

let expect_string (key, default) (global_data : env) =
  match String_hashtbl.find_exn global_data key with 
  | exception Not_found -> default
  | Atom s | Lit s -> s 
  | List _ | Data _ -> error (key, String)

let expect_string_opt key (global_data : env) =
  match String_hashtbl.find_exn global_data key with 
  | exception Not_found -> None
  | Atom s | Lit s -> Some s 
  | List _ | Data _ -> error (key, String)

let expect_string_list key (global_data : env) = 
  match String_hashtbl.find_exn global_data key  with 
  | exception Not_found -> [ ]
  | Atom _ | Lit _ | Data _ -> error(key, List String)
  | List xs -> 
    Ext_list.filter_map (fun x -> 
        match  x with 
        | Sexp_lexer.Atom x | Lit x -> Some x 
        | _ -> None 
      ) xs 

let expect_string_list_unordered 
  key 
  (global_data : env) 
  init update = 
  match String_hashtbl.find_exn global_data key with 
  | exception Not_found -> init
  | Atom _ | Lit _ 
  | Data _ -> error(key, List String)
  | List files -> 
    List.fold_left (fun acc s ->
        match s with 
        | Sexp_lexer.Atom s | Lit s -> update acc s 
        | _ -> acc (* raise Type error *)
      ) init files 

(* let rec expect_file_groups  key (global_data : env) = *)
(*   match String_hashtbl.find global_data key with  *)
(*   | exception Not_found -> [] *)
(*   | Atom _ | Lit _  *)
(*   | Data _ -> error (key, List Any) *)
(*   | List ls -> Ext_list.map expect_file_group  ls  *)

(* and expect_file_group (x : Sexp_lexer.t)  = *)
(*   match x with  *)
(*   | List [ List [ Atom "dir"; Lit dir ] ; List [ Atom "sources";  List files] ] ->  *)
(*     ( dir , *)
(*        List.fold_left (fun acc s -> *)
(*         match s with  *)
(*         | Sexp_lexer.Atom s | Lit s -> map_update ~dir  acc s  *)
(*         | _ -> acc (\* raise Type error *\) *)
(*       ) String_map.empty files *)
(*     ) *)

(*   | _ -> error ("", List Any) *)
