(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)
(* Adapted for Javascript backend : Hongbo Zhang,  *)




exception Real_reference

let rec eliminate_ref id (lam : Lam.t) = 
  match lam with  (** we can do better escape analysis in Javascript backend *)
  | Lvar v ->
    if Ident.same v id then raise_notrace Real_reference else lam
  | Lprim {primitive = Pfield (0,_); args =  [Lvar v]} when Ident.same v id ->
    Lam.var id
  | Lfunction{ function_kind; params; body} as lam ->
    if Ident_set.mem id (Lam.free_variables  lam)
    then raise_notrace Real_reference
    else lam
  (* In Javascript backend, its okay, we can reify it later
     a failed case 
     {[
       for i = .. 
           let v = ref 0 
               for j = .. 
                   incr v 
                     a[j] = ()=>{!v}

     ]}
     here v is captured by a block, and it's a loop mutable value,
     we have to generate 
     {[
       for i = .. 
           let v = ref 0 
               (function (v){for j = .. 
                                   a[j] = ()=>{!v}}(v)

     ]}
     now, v is a real reference 
     TODO: we can refine analysis in later
  *)
  (* Lfunction(kind, params, eliminate_ref id body) *)
  | Lprim {primitive = Psetfield(0, _,_); 
           args =  [Lvar v; e]} when Ident.same v id ->
    Lam.assign id (eliminate_ref id e)
  | Lprim {primitive = Poffsetref delta ; 
           args =  [Lvar v]; loc } when Ident.same v id ->
    Lam.assign id (Lam.prim ~primitive:(Poffsetint delta) ~args:[Lam.var id] loc)
  | Lconst _  -> lam
  | Lapply{fn = e1; args =  el;  loc; status} ->
    Lam.apply 
      (eliminate_ref id e1)
      (Ext_list.map (eliminate_ref id) el)
      loc status
  | Llet(str, v, e1, e2) ->
    Lam.let_ str v (eliminate_ref id e1) (eliminate_ref id e2)
  | Lletrec(idel, e2) ->
    Lam.letrec
      (Ext_list.map (fun (v, e) -> (v, eliminate_ref id e)) idel)
      (eliminate_ref id e2)
  | Lam.Lglobal_module _ -> lam     
  | Lprim {primitive ; args ; loc} ->
    Lam.prim  ~primitive ~args:(Ext_list.map (eliminate_ref id) args) loc
  | Lswitch(e, sw) ->
    Lam.switch(eliminate_ref id e)
      {sw_numconsts = sw.sw_numconsts;
       sw_consts =
         Ext_list.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_consts;
       sw_numblocks = sw.sw_numblocks;
       sw_blocks =
         Ext_list.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_blocks;
       sw_failaction =
         match sw.sw_failaction with 
         | None -> None 
         | Some x -> Some (eliminate_ref id x)
          }
  | Lstringswitch(e, sw, default) ->
    Lam.stringswitch
      (eliminate_ref id e)
      (Ext_list.map (fun (s, e) -> (s, eliminate_ref id e)) sw)
      (match default with 
      | None -> None 
      | Some x ->  Some (eliminate_ref id x))
  | Lstaticraise (i,args) ->
    Lam.staticraise i (Ext_list.map (eliminate_ref id) args)
  | Lstaticcatch(e1, i, e2) ->
    Lam.staticcatch (eliminate_ref id e1) i (eliminate_ref id e2)
  | Ltrywith(e1, v, e2) ->
    Lam.try_ (eliminate_ref id e1) v (eliminate_ref id e2)
  | Lifthenelse(e1, e2, e3) ->
    Lam.if_ (eliminate_ref id e1) (eliminate_ref id e2) (eliminate_ref id e3)
  | Lsequence(e1, e2) ->
    Lam.seq (eliminate_ref id e1) (eliminate_ref id e2)
  | Lwhile(e1, e2) ->
    Lam.while_ (eliminate_ref id e1) (eliminate_ref id e2)
  | Lfor(v, e1, e2, dir, e3) ->
    Lam.for_ v
      (eliminate_ref id e1) 
      (eliminate_ref id e2)
      dir
      (eliminate_ref id e3)
  | Lassign(v, e) ->
    Lam.assign v (eliminate_ref id e)
  | Lsend(k, m, o, el, loc) ->
    Lam.send k 
      (eliminate_ref id m) (eliminate_ref id o)
      (Ext_list.map (eliminate_ref id) el) loc
  | Lifused(v, e) ->
    Lam.ifused v (eliminate_ref id e)


