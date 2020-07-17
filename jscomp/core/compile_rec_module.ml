


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

let mod_prim name args loc =  
  if name = "init_mod" then
    bs_init_mod args loc
  else if name = "update_mod" then
    bs_update_mod args loc
  else assert false

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
                  mod_prim "init_mod" [loc; shape] Location.none,       
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
        mod_prim "update_mod" [shape; Lvar id; rhs] Location.none,
        patch_forwards rem)
  in
  bind_inits bindings 
    (bind_strict bindings 
       (patch_forwards bindings))

let rec aux (lam : Lambda.lambda) = 
  match lam with 
  | Lprim(Pmakeblock _, args , _)
    -> Ext_list.for_all args (fun x -> match x with Lvar _ -> true | _ -> false)     
  | Llet  (_,_,_,Lfunction _, cont)
    -> aux cont 
  | _ -> false     

let is_strict_or_all_functions (xs : binding list) = 
    Ext_list.for_all xs (fun (_, opt, rhs) -> 
      match opt with 
      | None -> true 
      | _ -> aux rhs
    )
    

let eval_rec_bindings 
    (bindings : binding list) 
    (cont : t) : t  =
  if is_strict_or_all_functions bindings then 
    Lambda.Lletrec 
      (Ext_list.map bindings (fun (id,_,rhs) -> id,rhs),cont  )
  else 
    eval_rec_bindings_aux bindings cont