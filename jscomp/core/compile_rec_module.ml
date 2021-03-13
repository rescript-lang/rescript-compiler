


type t = Lambda.lambda

(* Utilities for compiling "module rec" definitions *)

let bs_init_mod (args : t list) loc : t =  
  Lprim(Pccall (Primitive.simple 
                  ~name:"#init_mod"
                  ~arity:2
                  ~alloc:true), args, loc)

let bs_update_mod (args : t list) loc : t =
  Lprim(Pccall (Primitive.simple
                  ~name:"#update_mod"
                  ~arity:3
                  ~alloc:true), args, loc)



type loc = t 
type shape = t 

type binding = 
  Ident.t * (loc * shape) option * t


let eval_rec_bindings_aux 
    (bindings : binding list) (cont : t) : t =
  let rec bind_inits args acc = 
    match args with   
    |  [] -> acc 
    | (_id, None, _rhs) :: rem ->
      bind_inits rem acc 
    | (id, Some(loc, shape), _rhs) :: rem ->
      Lambda.Llet(Strict, Pgenval, id,
                  bs_init_mod [loc; shape] Location.none,       
                  bind_inits rem acc) in 
  let rec  bind_strict args acc = 
    match args with 
    |  [] -> acc 
    | (id, None, rhs) :: rem ->
      Lambda.Llet(Strict, Pgenval, id, rhs, bind_strict rem acc)
    | (_id, Some _, _rhs) :: rem ->
      bind_strict rem acc in 
  let rec patch_forwards args = 
    match args with 
    |  [] ->
      cont
    | (_id, None, _rhs) :: rem ->
      patch_forwards rem
    | (id, Some(_loc, shape), rhs) :: rem ->
      Lsequence(
        bs_update_mod [shape; Lvar id; rhs] Location.none,
        patch_forwards rem)
  in
  bind_inits bindings 
    (bind_strict bindings 
       (patch_forwards bindings))

(* collect all function declarations
    if the module creation is just a set of function declarations and consts,
    it is good
*)
let rec is_function_or_const_block (lam : Lambda.lambda) acc = 
  match lam with 
  | Lprim(Pmakeblock _, args , _)
    -> 
    Ext_list.for_all args 
      (fun x -> 
         match x with 
         | Lvar id -> Set_ident.mem acc id 
         | Lfunction _
         | Lconst _ -> true
         | _ -> false)     
  | Llet  (_,_,id,Lfunction _, cont)
    -> is_function_or_const_block cont (Set_ident.add acc id )
  | Lletrec (bindings, cont) -> 
    let rec aux_bindings bindings acc = 
      match bindings with 
      | [] -> Some acc
      | (id, Lambda.Lfunction _) :: rest  ->   
        aux_bindings rest (Set_ident.add acc id)
      | (_, _) :: _ -> 
        None
    in   
    begin match aux_bindings bindings acc with 
      | None -> false 
      | Some acc -> is_function_or_const_block cont acc 
    end 
  | Llet  (_,_,_,Lconst _, cont)
    -> is_function_or_const_block cont acc
  | Llet  (_,_, id1,Lvar id2, cont)
    when Set_ident.mem acc id2 ->
    is_function_or_const_block cont (Set_ident.add acc id1) 
  | _ -> false     


let is_strict_or_all_functions (xs : binding list) = 
  Ext_list.for_all xs (fun (_, opt, rhs) -> 
      match opt with 
      | None -> true 
      | _ -> is_function_or_const_block rhs Set_ident.empty
    )


(* Without such optimizations:

   {[
     module rec X : sig 
       val f : int -> int   
     end = struct 
       let f x = x + 1
     end   
     and Y : sig 
       val f : int -> int  
     end = struct 
       let f x  = x + 2
     end
   ]}
   would generate such rawlambda:

   {[
     (setglobal Debug_tmp!
        (let
          (X/1002 = (#init_mod [0: "debug_tmp.ml" 15 6] [0: [0: [0: 0a "f"]]])
             Y/1003 = (#init_mod [0: "debug_tmp.ml" 20 6] [0: [0: [0: 0a "f"]]]))
            (seq
               (#update_mod [0: [0: [0: 0a "f"]]] X/1002
                  (let (f/1010 = (function x/1011 (+ x/1011 1)))
                       (makeblock 0/[f] f/1010)))
               (#update_mod [0: [0: [0: 0a "f"]]] Y/1003
                  (let (f/1012 = (function x/1013 (+ x/1013 2)))
                       (makeblock 0/[f] f/1012)))
               (makeblock 0/module/exports X/1002 Y/1003))))

   ]}
*)
let eval_rec_bindings 
    (bindings : binding list) 
    (cont : t) : t  =
  if  is_strict_or_all_functions bindings then 
    Lambda.Lletrec 
      (Ext_list.map bindings (fun (id,_,rhs) -> id,rhs),cont  )
  else 
    eval_rec_bindings_aux bindings cont