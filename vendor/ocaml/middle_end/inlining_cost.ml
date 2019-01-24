(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(* Simple approximation of the space cost of a primitive. *)

let prim_size (prim : Lambda.primitive) args =
  match prim with
  | Pidentity -> 0
  | Pgetglobal _ -> 1
  | Psetglobal _ -> 1
  | Pmakeblock _ -> 5 + List.length args
  | Pfield _ -> 1
  | Psetfield (_, isptr, init,_) ->
    begin match init with
    | Root_initialization -> 1  (* never causes a write barrier hit *)
    | Assignment | Heap_initialization ->
      match isptr with
      | Pointer -> 4
      | Immediate -> 1
    end
  | Pfloatfield _ -> 1
  | Psetfloatfield _ -> 1
  | Pduprecord _ -> 10 + List.length args
  | Pccall p -> (if p.Primitive.prim_alloc then 10 else 4) + List.length args
  | Praise _ -> 4
  | Pstringlength -> 5
  | Pbyteslength -> 5
  | Pstringrefs -> 6
  | Pbytesrefs | Pbytessets -> 6
  | Pmakearray _ -> 5 + List.length args
  | Parraylength Pgenarray -> 6
  | Parraylength _ -> 2
  | Parrayrefu Pgenarray -> 12
  | Parrayrefu _ -> 2
  | Parraysetu Pgenarray -> 16
  | Parraysetu _ -> 4
  | Parrayrefs Pgenarray -> 18
  | Parrayrefs _ -> 8
  | Parraysets Pgenarray -> 22
  | Parraysets _ -> 10
  | Pbittest -> 3
  | Pbigarrayref (_, ndims, _, _) -> 4 + ndims * 6
  | Pbigarrayset (_, ndims, _, _) -> 4 + ndims * 6
  | Psequand | Psequor ->
    Misc.fatal_error "Psequand and Psequor are not allowed in Prim \
        expressions; translate out instead (cf. closure_conversion.ml)"
  (* CR-soon mshinwell: This match must be made exhaustive.
     mshinwell: Let's do this when we have the new size computation. *)
  | _ -> 2 (* arithmetic and comparisons *)

(* Simple approximation of the space cost of an Flambda expression. *)

(* CR-soon mshinwell: Investigate revised size numbers. *)

let direct_call_size = 4
let project_size = 1

let lambda_smaller' lam ~than:threshold =
  let size = ref 0 in
  let rec lambda_size (lam : Flambda.t) =
    if !size > threshold then raise Exit;
    match lam with
    | Var _ -> ()
    | Apply ({ func = _; args = _; kind = direct }) ->
      let call_cost =
        match direct with Indirect -> 6 | Direct _ -> direct_call_size
      in
      size := !size + call_cost
    | Assign _ -> incr size
    | Send _ -> size := !size + 8
    | Proved_unreachable -> ()
    | Let { defining_expr; body; _ } ->
      lambda_named_size defining_expr;
      lambda_size body
    | Let_mutable { body } -> lambda_size body
    | Let_rec (bindings, body) ->
      List.iter (fun (_, lam) -> lambda_named_size lam) bindings;
      lambda_size body
    | Switch (_, sw) ->
      let aux = function _::_::_ -> size := !size + 5 | _ -> () in
      aux sw.consts; aux sw.blocks;
      List.iter (fun (_, lam) -> lambda_size lam) sw.consts;
      List.iter (fun (_, lam) -> lambda_size lam) sw.blocks;
      Misc.Stdlib.Option.iter lambda_size sw.failaction
    | String_switch (_, sw, def) ->
      List.iter (fun (_, lam) ->
          size := !size + 2;
          lambda_size lam)
        sw;
      Misc.may lambda_size def
    | Static_raise _ -> ()
    | Static_catch (_, _, body, handler) ->
      incr size; lambda_size body; lambda_size handler
    | Try_with (body, _, handler) ->
      size := !size + 8; lambda_size body; lambda_size handler
    | If_then_else (_, ifso, ifnot) ->
      size := !size + 2;
      lambda_size ifso; lambda_size ifnot
    | While (cond, body) ->
      size := !size + 2; lambda_size cond; lambda_size body
    | For { body; _ } ->
      size := !size + 4; lambda_size body
  and lambda_named_size (named : Flambda.named) =
    if !size > threshold then raise Exit;
    match named with
    | Symbol _ | Read_mutable _ -> ()
    | Const _ | Allocated_const _ -> incr size
    | Read_symbol_field _ -> incr size
    | Set_of_closures ({ function_decls = ffuns }) ->
      Variable.Map.iter (fun _ (ffun : Flambda.function_declaration) ->
          lambda_size ffun.body)
        ffuns.funs
    | Project_closure _ | Project_var _ ->
      size := !size + project_size
    | Move_within_set_of_closures _ ->
      incr size
    | Prim (prim, args, _) ->
      size := !size + prim_size prim args
    | Expr expr -> lambda_size expr
  in
  try
    lambda_size lam;
    if !size <= threshold then Some !size
    else None
  with Exit ->
    None

let lambda_size lam =
  match lambda_smaller' lam ~than:max_int with
  | Some size ->
      size
  | None ->
      (* There is no way that an expression of size max_int could fit in
         memory. *)
      assert false

module Threshold = struct

  type t =
    | Never_inline
    | Can_inline_if_no_larger_than of int

  let add t1 t2 =
    match t1, t2 with
    | Never_inline, t -> t
    | t, Never_inline -> t
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
        Can_inline_if_no_larger_than (i1 + i2)

  let sub t1 t2 =
    match t1, t2 with
    | Never_inline, _ -> Never_inline
    | t, Never_inline -> t
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
        if i1 > i2 then Can_inline_if_no_larger_than (i1 - i2)
        else Never_inline

  let min t1 t2 =
    match t1, t2 with
    | Never_inline, _ -> Never_inline
    | _, Never_inline -> Never_inline
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
      Can_inline_if_no_larger_than (min i1 i2)

end

let can_try_inlining lam inlining_threshold ~number_of_arguments
      ~size_from_approximation =
  match inlining_threshold with
  | Threshold.Never_inline -> Threshold.Never_inline
  | Threshold.Can_inline_if_no_larger_than inlining_threshold ->
    let bonus =
      (* removing a call will reduce the size by at least the number
         of arguments *)
      number_of_arguments
    in
    let size =
      let than = inlining_threshold + bonus in
      match size_from_approximation with
      | Some size -> if size <= than then Some size else None
      | None -> lambda_smaller' lam ~than
    in
    match size with
    | None -> Threshold.Never_inline
    | Some size ->
      Threshold.Can_inline_if_no_larger_than
        (inlining_threshold - size + bonus)

let lambda_smaller lam ~than =
  lambda_smaller' lam ~than <> None

let can_inline lam inlining_threshold ~bonus =
  match inlining_threshold with
  | Threshold.Never_inline -> false
  | Threshold.Can_inline_if_no_larger_than inlining_threshold ->
     lambda_smaller
       lam
       ~than:(inlining_threshold + bonus)

let cost (flag : Clflags.Int_arg_helper.parsed) ~round =
  Clflags.Int_arg_helper.get ~key:round flag

let benefit_factor = 1

module Benefit = struct
  type t = {
    remove_call : int;
    remove_alloc : int;
    remove_prim : int;
    remove_branch : int;
    (* CR-someday pchambart: branch_benefit : t list; *)
    direct_call_of_indirect : int;
    requested_inline : int;
    (* Benefit to compensate the size of functions marked for inlining *)
  }

  let zero = {
    remove_call = 0;
    remove_alloc = 0;
    remove_prim = 0;
    remove_branch = 0;
    direct_call_of_indirect = 0;
    requested_inline = 0;
  }

  let remove_call t = { t with remove_call = t.remove_call + 1; }
  let remove_alloc t = { t with remove_alloc = t.remove_alloc + 1; }
  let remove_prim t = { t with remove_prim = t.remove_prim + 1; }
  let remove_prims t n = { t with remove_prim = t.remove_prim + n; }
  let remove_branch t = { t with remove_branch = t.remove_branch + 1; }
  let direct_call_of_indirect t =
    { t with direct_call_of_indirect = t.direct_call_of_indirect + 1; }
  let requested_inline t ~size_of =
    let size = lambda_size size_of in
    { t with requested_inline = t.requested_inline + size; }

  let remove_code_helper b (flam : Flambda.t) =
    match flam with
    | Assign _ -> b := remove_prim !b
    | Switch _ | String_switch _ | Static_raise _ | Try_with _
    | If_then_else _ | While _ | For _ -> b := remove_branch !b
    | Apply _ | Send _ -> b := remove_call !b
    | Let _ | Let_mutable _ | Let_rec _ | Proved_unreachable | Var _
    | Static_catch _ -> ()

  let remove_code_helper_named b (named : Flambda.named) =
    match named with
    | Set_of_closures _
    | Prim ((Pmakearray _ | Pmakeblock _ | Pduprecord _), _, _) ->
      b := remove_alloc !b
      (* CR-soon pchambart: should we consider that boxed integer and float
         operations are allocations ? *)
    | Prim _ | Project_closure _ | Project_var _
    | Move_within_set_of_closures _
    | Read_symbol_field _ -> b := remove_prim !b
    | Symbol _ | Read_mutable _ | Allocated_const _ | Const _ | Expr _ -> ()

  let remove_code lam b =
    let b = ref b in
    Flambda_iterators.iter_toplevel (remove_code_helper b)
      (remove_code_helper_named b) lam;
    !b

  let remove_code_named lam b =
    let b = ref b in
    Flambda_iterators.iter_named_toplevel (remove_code_helper b)
      (remove_code_helper_named b) lam;
    !b

  let remove_projection (_proj : Projection.t) b =
    (* They are all primitives for the moment.  The [Projection.t] argument
       is here for future expansion. *)
    remove_prim b

  let print ppf b =
    Format.fprintf ppf "@[remove_call: %i@ remove_alloc: %i@ \
                        remove_prim: %i@ remove_branch: %i@ \
                        direct: %i@ requested: %i@]"
      b.remove_call
      b.remove_alloc
      b.remove_prim
      b.remove_branch
      b.direct_call_of_indirect
      b.requested_inline

  let evaluate t ~round : int =
    benefit_factor *
      (t.remove_call * (cost !Clflags.inline_call_cost ~round)
       + t.remove_alloc * (cost !Clflags.inline_alloc_cost ~round)
       + t.remove_prim * (cost !Clflags.inline_prim_cost ~round)
       + t.remove_branch * (cost !Clflags.inline_branch_cost ~round)
       + (t.direct_call_of_indirect
         * (cost !Clflags.inline_indirect_cost ~round)))
    + t.requested_inline

  let (+) t1 t2 = {
    remove_call = t1.remove_call + t2.remove_call;
    remove_alloc = t1.remove_alloc + t2.remove_alloc;
    remove_prim = t1.remove_prim + t2.remove_prim;
    remove_branch = t1.remove_branch + t2.remove_branch;
    direct_call_of_indirect =
      t1.direct_call_of_indirect + t2.direct_call_of_indirect;
    requested_inline = t1.requested_inline + t2.requested_inline;
  }

  let (-) t1 t2 = {
    remove_call = t1.remove_call - t2.remove_call;
    remove_alloc = t1.remove_alloc - t2.remove_alloc;
    remove_prim = t1.remove_prim - t2.remove_prim;
    remove_branch = t1.remove_branch - t2.remove_branch;
    direct_call_of_indirect =
      t1.direct_call_of_indirect - t2.direct_call_of_indirect;
    requested_inline = t1.requested_inline - t2.requested_inline;
  }

  let max ~round t1 t2 =
    let c1 = evaluate ~round t1 in
    let c2 = evaluate ~round t2 in
    if c1 > c2 then t1 else t2

  let add_code lam b =
    b - (remove_code lam zero)

  let add_code_named lam b =
    b - (remove_code_named lam zero)

  let add_projection proj b =
    b - (remove_projection proj zero)

  (* Print out a benefit as a table *)

  let benefit_table =
    [ "Calls", (fun b -> b.remove_call);
      "Allocs", (fun b -> b.remove_alloc);
      "Prims", (fun b -> b.remove_prim);
      "Branches", (fun b -> b.remove_branch);
      "Indirect calls", (fun b -> b.direct_call_of_indirect);
    ]

  let benefits_table =
    lazy begin
      List.map
        (fun (header, accessor) -> (header, accessor, String.length header))
        benefit_table
    end

  let table_line =
    lazy begin
      let benefits_table = Lazy.force benefits_table in
      let dashes =
        List.map (fun (_, _, n) -> String.make n '-') benefits_table
      in
      "|-" ^ String.concat "-+-" dashes ^ "-|"
    end

  let table_headers =
    lazy begin
      let benefits_table = Lazy.force benefits_table in
      let headers = List.map (fun (head, _, _) -> head) benefits_table in
      "| " ^ String.concat " | " headers ^ " |"
    end

  let print_table_values ppf b =
    let rec loop ppf = function
      | [] -> Format.fprintf ppf "|"
      | (_, accessor, width) :: rest ->
        Format.fprintf ppf "| %*d %a" width (accessor b) loop rest
    in
    loop ppf (Lazy.force benefits_table)

  let print_table ppf b =
    let table_line = Lazy.force table_line in
    let table_headers = Lazy.force table_headers in
    Format.fprintf ppf
      "@[<v>@[<h>%s@]@;@[<h>%s@]@;@[<h>%s@]@;@[<h>%a@]@;@[<h>%s@]@]"
      table_line table_headers table_line
      print_table_values b
      table_line
end

module Whether_sufficient_benefit = struct
  type t = {
    round : int;
    benefit : Benefit.t;
    toplevel : bool;
    branch_depth : int;
    lifting : bool;
    original_size : int;
    new_size : int;
    evaluated_benefit : int;
    estimate : bool;
  }

  let create ~original ~toplevel ~branch_depth lam ~benefit ~lifting ~round =
    let evaluated_benefit = Benefit.evaluate benefit ~round in
    { round; benefit; toplevel; branch_depth; lifting;
      original_size = lambda_size original;
      new_size = lambda_size lam;
      evaluated_benefit;
      estimate = false;
    }

  let create_estimate ~original_size ~toplevel ~branch_depth ~new_size
        ~benefit ~lifting ~round =
    let evaluated_benefit = Benefit.evaluate benefit ~round in
    { round; benefit; toplevel; branch_depth; lifting; original_size;
      new_size; evaluated_benefit; estimate = true;
    }

  let correct_branch_factor f =
    f = f (* is not nan *)
    && f >= 0.

  let estimated_benefit t =
    if t.toplevel && t.lifting && t.branch_depth = 0 then begin
      let lifting_benefit =
        Clflags.Int_arg_helper.get ~key:t.round !Clflags.inline_lifting_benefit
      in
        float (t.evaluated_benefit + lifting_benefit)
    end else begin
      (* The estimated benefit is the evaluated benefit times an
         estimation of the probability that the branch does actually matter
         for performance (i.e. is hot).  The probability is very roughly
         estimated by considering that under every branch the
         sub-expressions have the same [1 / (1 + factor)] probability
         [p] of being hot.  Hence the probability for the current
         call to be hot is [p ^ number of nested branches].
         The probability is expressed as [1 / (1 + factor)] rather
         than letting the user directly provide [p], since for every
         positive value of [factor] [p] is in [0, 1]. *)
      let branch_taken_estimated_probability =
        let inline_branch_factor =
          let factor =
            Clflags.Float_arg_helper.get ~key:t.round
              !Clflags.inline_branch_factor
          in
          if not (factor = factor) (* nan *) then
            Clflags.default_inline_branch_factor
          else if factor < 0. then
            0.
          else
            factor
        in
        assert (correct_branch_factor inline_branch_factor);
        1. /. (1. +. inline_branch_factor)
      in
      let call_estimated_probability =
        branch_taken_estimated_probability ** float t.branch_depth
      in
      float t.evaluated_benefit *. call_estimated_probability
    end

  let evaluate t =
    float t.new_size -. estimated_benefit t <= float t.original_size

  let to_string t =
    let lifting = t.toplevel && t.lifting && t.branch_depth = 0 in
    let evaluated_benefit =
      if lifting then
        let lifting_benefit =
          Clflags.Int_arg_helper.get ~key:t.round
            !Clflags.inline_lifting_benefit
        in
        t.evaluated_benefit + lifting_benefit
      else t.evaluated_benefit
    in
    let estimate = if t.estimate then "<" else "=" in
      Printf.sprintf "{benefit%s{call=%d,alloc=%d,prim=%i,branch=%i,\
          indirect=%i,req=%i,\
          lifting=%B}, orig_size=%d,new_size=%d,eval_size=%d,\
          eval_benefit%s%d,\
          branch_depth=%d}=%s"
        estimate
        t.benefit.remove_call
        t.benefit.remove_alloc
        t.benefit.remove_prim
        t.benefit.remove_branch
        t.benefit.direct_call_of_indirect
        t.benefit.requested_inline
        lifting
        t.original_size
        t.new_size
        (t.original_size - t.new_size)
        estimate
        evaluated_benefit
        t.branch_depth
        (if evaluate t then "yes" else "no")

  let print_description ~subfunctions ppf t =
    let pr_intro ppf =
      let estimate = if t.estimate then " at most" else "" in
      Format.pp_print_text ppf
        "Specialisation of the function body";
      if subfunctions then
        Format.pp_print_text ppf
          ", including speculative inlining of other functions,";
      Format.pp_print_text ppf " removed";
      Format.pp_print_text ppf estimate;
      Format.pp_print_text ppf " the following operations:"
    in
    let lifting = t.toplevel && t.lifting && t.branch_depth = 0 in
    let requested = t.benefit.requested_inline in
    let pr_requested ppf =
      if requested > 0 then begin
        Format.pp_open_box ppf 0;
        Format.pp_print_text ppf
            "and inlined user-annotated functions worth ";
        Format.fprintf ppf "%d." requested;
        Format.pp_close_box ppf ();
        Format.pp_print_cut ppf ();
        Format.pp_print_cut ppf ()
      end
    in
    let pr_lifting ppf =
      if lifting then begin
        Format.pp_open_box ppf 0;
        Format.pp_print_text ppf
          "Inlining the function would also \
           lift some definitions to toplevel.";
        Format.pp_close_box ppf ();
        Format.pp_print_cut ppf ();
        Format.pp_print_cut ppf ()
      end
    in
    let total_benefit =
      if lifting then
        let lifting_benefit =
          Clflags.Int_arg_helper.get ~key:t.round
            !Clflags.inline_lifting_benefit
        in
         t.evaluated_benefit + lifting_benefit
      else t.evaluated_benefit
    in
    let expected_benefit = estimated_benefit t in
    let size_change = t.new_size - t.original_size in
    let result = if evaluate t then "less" else "greater" in
    let pr_conclusion ppf =
      Format.pp_print_text ppf "This gives a total benefit of ";
      Format.pp_print_int ppf total_benefit;
      Format.pp_print_text ppf ".  At a branch depth of ";
      Format.pp_print_int ppf t.branch_depth;
      Format.pp_print_text ppf " this produces an expected benefit of ";
      Format.fprintf ppf "%.1f" expected_benefit;
      Format.pp_print_text ppf ".  The new code has size ";
      Format.pp_print_int ppf t.new_size;
      Format.pp_print_text ppf ", giving a change in code size of ";
      Format.pp_print_int ppf size_change;
      Format.pp_print_text ppf ".  The change in code size is ";
      Format.pp_print_text ppf result;
      Format.pp_print_text ppf " than the expected benefit."
    in
    Format.fprintf ppf "%t@,@[<v>@[<v 2>@;%a@]@;@;%t%t@]%t"
      pr_intro Benefit.print_table t.benefit pr_requested pr_lifting
      pr_conclusion
end

let scale_inline_threshold_by = 8

let default_toplevel_multiplier = 8

  (* CR-soon mshinwell for mshinwell: hastily-written comment, to review *)
  (* We may in [Inlining_decision] need to measure the size of functions
     that are below the inlining threshold.  We also need to measure with
     regard to benefit (see [Inlining_decision.inline_non_recursive).  The
     intuition for having a cached size in the second case is as follows.
     If a function's body exceeds some maximum size and its argument
     approximations are unknown (meaning that we cannot materially simplify
     it further), we can infer without examining the function's body that
     it cannot be inlined.  The aim is to speed up [Inlining_decision].

     The "original size" is [Inlining_cost.direct_call_size].  The "new size" is
     the size of the function's body plus [Inlining_cost.project_size] for each
     free variable and mutually recursive function accessed through the closure.

     To be inlined we need:

       body_size
       + (closure_accesses * project_size)            <=   direct_call_size
       - (evaluated_benefit * call_prob)

     i.e.:

       body_size <= direct_call_size
                    + (evaluated_benefit * call_prob)
                    - (closure_accesses * project_size)

     In this case we would be removing a single call and a projection for each
     free variable that can be accessed directly (i.e. not via the closure
     or the internal variable).

       evaluated_benefit =
         benefit_factor
         * (inline_call_cost
         + ((free_variables - indirect_accesses) * inline_prim_cost))

     (For [inline_call_cost] and [inline_prim_cost], we use the maximum these
     might be across any round.)

     Substituting:

       body_size <= direct_call_size
                      + (benefit_factor
                          * (inline_call_cost
                             + ((free_variables - indirect_accesses)
                                * inline_prim_cost)))
                        * call_prob
                      - (closure_accesses * project_size)

     Rearranging:

       body_size <= direct_call_size
                      + (inline_call_cost * benefit_factor * call_prob)
                      + (free_variables * inline_prim_cost
                           * benefit_factor * call_prob)
                      - (indirect_accesses * inline_prim_cost
                           * benefit_factor * call_prob)
                      - (closure_accesses * project_size)

     The upper bound for the right-hand side is when call_prob = 1.0,
     indirect_accesses = 0 and closure_accesses = 0, giving:

       direct_call_size
         + (inline_call_cost * benefit_factor)
         + (free_variables * inline_prim_cost * benefit_factor)

     So we should measure all functions at or below this size, but also record
     the size discovered, so we can later re-check (without examining the body)
     when we know [call_prob], [indirect_accesses] and [closure_accesses].

     This number is split into parts dependent and independent of the
     number of free variables:

       base = direct_call_size + (inline_call_cost * benefit_factor)

       multiplier = inline_prim_cost * benefit_factor

       body_size <= base + free_variables * multiplier

  *)
let maximum_interesting_size_of_function_body_base =
  lazy begin
    let max_cost = ref 0 in
    for round = 0 to (Clflags.rounds ()) - 1 do
      let max_size =
        let inline_call_cost = cost !Clflags.inline_call_cost ~round in
        direct_call_size + (inline_call_cost * benefit_factor)
      in
      max_cost := max !max_cost max_size
    done;
    !max_cost
  end

let maximum_interesting_size_of_function_body_multiplier =
  lazy begin
    let max_cost = ref 0 in
    for round = 0 to (Clflags.rounds ()) - 1 do
      let max_size =
        let inline_prim_cost = cost !Clflags.inline_prim_cost ~round in
        inline_prim_cost * benefit_factor
      in
      max_cost := max !max_cost max_size
    done;
    !max_cost
  end

let maximum_interesting_size_of_function_body num_free_variables =
  let base = Lazy.force maximum_interesting_size_of_function_body_base in
  let multiplier =
    Lazy.force maximum_interesting_size_of_function_body_multiplier
  in
  base + (num_free_variables * multiplier)
