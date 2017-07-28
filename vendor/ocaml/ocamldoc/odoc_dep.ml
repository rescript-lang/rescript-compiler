(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Top modules dependencies. *)

module StrS = Depend.StringSet
module Module = Odoc_module
module Type = Odoc_type

let set_to_list s =
  let l = ref [] in
  StrS.iter (fun e -> l := e :: !l) s;
  !l

let impl_dependencies ast =
  Depend.free_structure_names := StrS.empty;
  Depend.add_use_file StrS.empty [Parsetree.Ptop_def ast];
  set_to_list !Depend.free_structure_names

let intf_dependencies ast =
  Depend.free_structure_names := StrS.empty;
  Depend.add_signature StrS.empty ast;
  set_to_list !Depend.free_structure_names


module Dep =
  struct
    type id = string

    module S = Set.Make (struct
      type t = string
      let compare (x:t) y = compare x y
    end)

    let set_to_list s =
      let l = ref [] in
      S.iter (fun e -> l := e :: !l) s;
      !l

    type node = {
        id : id ;
        mutable near : S.t ; (** fils directs *)
        mutable far : (id * S.t) list ; (** fils indirects, par quel fils *)
        reflex : bool ; (** reflexive or not, we keep
                           information here to remove the node itself from its direct children *)
      }

    type graph = node list

    let make_node s children =
      let set = List.fold_right
          S.add
          children
          S.empty
      in
      { id = s;
        near = S.remove s set ;
        far = [] ;
        reflex = List.mem s children ;
      }

    let get_node graph s =
      try List.find (fun n -> n.id = s) graph
      with Not_found ->
        make_node s []

    let rec trans_closure graph acc n =
      if S.mem n.id acc then
        acc
      else
        (* optimisation plus tard : utiliser le champ far si non vide ? *)
        S.fold
          (fun child -> fun acc2 ->
            trans_closure graph acc2 (get_node graph child))
          n.near
          (S.add n.id acc)

    let node_trans_closure graph n =
      let far = List.map
          (fun child ->
            let set = trans_closure graph S.empty (get_node graph child) in
            (child, set)
          )
          (set_to_list n.near)
      in
      n.far <- far

    let compute_trans_closure graph =
      List.iter (node_trans_closure graph) graph

    let prune_node graph node =
      S.iter
        (fun child ->
          let set_reachables = List.fold_left
              (fun acc -> fun (ch, reachables) ->
                if child = ch then
                  acc
                else
                  S.union acc reachables
              )
              S.empty
              node.far
          in
          let set = S.remove node.id set_reachables in
          if S.exists (fun n2 -> S.mem child (get_node graph n2).near) set then
            (
             node.near <- S.remove child node.near ;
             node.far <- List.filter (fun (ch,_) -> ch <> child) node.far
            )
          else
            ()
        )
        node.near;
      if node.reflex then
        node.near <- S.add node.id node.near
      else
        ()

    let kernel graph =
      (* compute transitive closure *)
      compute_trans_closure graph ;

      (* remove edges to keep a transitive kernel *)
      List.iter (prune_node graph) graph;

      graph

  end

(** [type_deps t] returns the list of fully qualified type names
   [t] depends on. *)
let type_deps t =
  let module T = Odoc_type in
  let l = ref [] in
  let re = Str.regexp "\\([A-Z]\\([a-zA-Z_'0-9]\\)*\\.\\)+\\([a-z][a-zA-Z_'0-9]*\\)" in
  let f s =
    let s2 = Str.matched_string s in
    l := s2 :: !l ;
    s2
  in
  (match t.T.ty_kind with
    T.Type_abstract -> ()
  | T.Type_variant cl ->
      List.iter
        (fun c ->
          List.iter
            (fun e ->
              let s = Odoc_print.string_of_type_expr e in
              ignore (Str.global_substitute re f s)
            )
            c.T.vc_args
        )
        cl
  | T.Type_record rl ->
      List.iter
        (fun r ->
          let s = Odoc_print.string_of_type_expr r.T.rf_type in
          ignore (Str.global_substitute re f s)
        )
        rl
  | T.Type_open -> ()
  );

  (match t.T.ty_manifest with
    None -> ()
  | Some (T.Object_type fields) ->
      List.iter
        (fun r ->
          let s = Odoc_print.string_of_type_expr r.T.of_type in
          ignore (Str.global_substitute re f s)
          )
        fields
  | Some (T.Other e) ->
      let s = Odoc_print.string_of_type_expr e in
      ignore (Str.global_substitute re f s)
  );

  !l

(** Modify the modules depencies of the given list of modules,
   to get the minimum transitivity kernel. *)
let kernel_deps_of_modules modules =
  let graph = List.map
      (fun m -> Dep.make_node m.Module.m_name m.Module.m_top_deps)
      modules
  in
  let k = Dep.kernel graph in
  List.iter
    (fun m ->
      let node = Dep.get_node k m.Module.m_name in
      m.Module.m_top_deps <-
        List.filter (fun m2 -> Dep.S.mem m2 node.Dep.near) m.Module.m_top_deps)
    modules

(** Return the list of dependencies between the given types,
   in the form of a list [(type, names of types it depends on)].
   @param kernel indicates if we must keep only the transitivity kernel
   of the dependencies. Default is [false].
*)
let deps_of_types ?(kernel=false) types =
  let deps_pre = List.map (fun t -> (t, type_deps t)) types in
  let deps =
    if kernel then
      (
       let graph = List.map
           (fun (t, names) -> Dep.make_node t.Type.ty_name names)
           deps_pre
       in
       let k = Dep.kernel graph in
       List.map
         (fun t ->
           let node = Dep.get_node k t.Type.ty_name in
           (t, Dep.set_to_list node.Dep.near)
         )
         types
      )
    else
      deps_pre
  in
  deps
