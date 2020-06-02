

open Lambda

(* Utilities for compiling "module rec" definitions *)
let bs_init_mod args loc : Lambda.lambda =  
  Lprim(Pccall (Primitive.simple 
                  ~name:"#init_mod"
                  ~arity:2
                  ~alloc:true), args, loc)
let bs_update_mod args loc : Lambda.lambda =
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

type binding = 
  (Ident.t * (Lambda.lambda * Lambda.lambda) option * Lambda.lambda)  

let eval_rec_bindings_aux 
    (bindings : binding list) (cont : Lambda.lambda) =
  let rec bind_inits = function
    [] ->
      bind_strict bindings
  | (_id, None, _rhs) :: rem ->
      bind_inits rem
  | (id, Some(loc, shape), _rhs) :: rem ->
      Llet(Strict, Pgenval, id,
           mod_prim "init_mod" [loc; shape] Location.none,       
           bind_inits rem)
  and bind_strict = function
    [] ->
      patch_forwards bindings
  | (id, None, rhs) :: rem ->
      Llet(Strict, Pgenval, id, rhs, bind_strict rem)
  | (_id, Some _, _rhs) :: rem ->
      bind_strict rem
  and patch_forwards = function
    [] ->
      cont
  | (_id, None, _rhs) :: rem ->
      patch_forwards rem
  | (id, Some(_loc, shape), rhs) :: rem ->
      Lsequence(
            mod_prim "update_mod" [shape; Lvar id; rhs] Location.none,
            patch_forwards rem)
  in
    bind_inits bindings

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
    (bindings : binding list) (cont : Lambda.lambda) =
  if is_strict_or_all_functions bindings then 
    Lletrec (Ext_list.map bindings (fun (id,_,rhs) -> id,rhs),cont  )
  else 
    eval_rec_bindings_aux bindings cont