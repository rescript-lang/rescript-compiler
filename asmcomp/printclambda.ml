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


open Format
open Asttypes
open Clambda

let rec structured_constant ppf = function
  | Uconst_float x -> fprintf ppf "%F" x
  | Uconst_int32 x -> fprintf ppf "%ldl" x
  | Uconst_int64 x -> fprintf ppf "%LdL" x
  | Uconst_nativeint x -> fprintf ppf "%ndn" x
  | Uconst_block (tag, l) ->
      fprintf ppf "block(%i" tag;
      List.iter (fun u -> fprintf ppf ",%a" uconstant u) l;
      fprintf ppf ")"
  | Uconst_float_array [] ->
      fprintf ppf "floatarray()"
  | Uconst_float_array (f1 :: fl) ->
      fprintf ppf "floatarray(%F" f1;
      List.iter (fun f -> fprintf ppf ",%F" f) fl;
      fprintf ppf ")"
  | Uconst_string s -> fprintf ppf "%S" s

and uconstant ppf = function
  | Uconst_ref (s, c) ->
      fprintf ppf "%S=%a" s structured_constant c
  | Uconst_int i -> fprintf ppf "%i" i
  | Uconst_ptr i -> fprintf ppf "%ia" i

let rec lam ppf = function
  | Uvar id ->
      Ident.print ppf id
  | Uconst c -> uconstant ppf c
  | Udirect_apply(f, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(apply*@ %s %a)@]" f lams largs
  | Ugeneric_apply(lfun, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(apply@ %a%a)@]" lam lfun lams largs
  | Uclosure(clos, fv) ->
      let idents ppf =
        List.iter (fprintf ppf "@ %a" Ident.print)in
      let one_fun ppf f =
        fprintf ppf "@[<2>(fun@ %s@ %d @[<2>%a@]@ @[<2>%a@]@])"
          f.label f.arity idents f.params lam f.body in
      let funs ppf =
        List.iter (fprintf ppf "@ %a" one_fun) in
      let lams ppf =
        List.iter (fprintf ppf "@ %a" lam) in
      fprintf ppf "@[<2>(closure@ %a %a)@]" funs clos lams fv
  | Uoffset(l,i) -> fprintf ppf "@[<2>(offset %a %d)@]" lam l i
  | Ulet(id, arg, body) ->
      let rec letbody ul = match ul with
        | Ulet(id, arg, body) ->
            fprintf ppf "@ @[<2>%a@ %a@]" Ident.print id lam arg;
            letbody body
        | _ -> ul in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]" Ident.print id lam arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Uletrec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a@ %a@]" Ident.print id lam l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Uprim(prim, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(%a%a)@]" Printlambda.primitive prim lams largs
  | Uswitch(larg, sw) ->
      let print_case tag index i ppf =
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then fprintf ppf "case %s %i:" tag j
        done in
      let print_cases tag index cases ppf =
        for i = 0 to Array.length cases - 1 do
          fprintf ppf "@ @[<2>%t@ %a@]"
            (print_case tag index i) sequence cases.(i)
        done in
      let switch ppf sw =
        print_cases "int" sw.us_index_consts sw.us_actions_consts ppf ;
        print_cases "tag" sw.us_index_blocks sw.us_actions_blocks ppf  in
      fprintf ppf
       "@[<v 0>@[<2>(switch@ %a@ @]%a)@]"
        lam larg switch sw
  | Ustringswitch(larg,sw,d) ->
      let switch ppf sw =
        let spc = ref false in
        List.iter
          (fun (s,l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>case \"%s\":@ %a@]"
              (String.escaped s) lam l)
          sw ;
        begin match d with
        | Some d ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam d
        | None -> ()
        end in
      fprintf ppf
        "@[<1>(switch %a@ @[<v 0>%a@])@]" lam larg switch sw
  | Ustaticfail (i, ls)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit@ %d%a)@]" i lams ls;
  | Ucatch(i, vars, lbody, lhandler) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%d%a)@ %a)@]"
        lam lbody i
        (fun ppf vars -> match vars with
          | [] -> ()
          | _ ->
              List.iter
                (fun x -> fprintf ppf " %a" Ident.print x)
                vars)
        vars
        lam lhandler
  | Utrywith(lbody, param, lhandler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Ident.print param lam lhandler
  | Uifthenelse(lcond, lif, lelse) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Usequence(l1, l2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Uwhile(lcond, lbody) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Ufor(param, lo, hi, dir, body) ->
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
       Ident.print param lam lo
       (match dir with Upto -> "to" | Downto -> "downto")
       lam hi lam body
  | Uassign(id, expr) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id lam expr
  | Usend (k, met, obj, largs, _) ->
      let args ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      let kind =
        if k = Lambda.Self then "self"
        else if k = Lambda.Cached then "cache"
        else "" in
      fprintf ppf "@[<2>(send%s@ %a@ %a%a)@]" kind lam obj lam met args largs

and sequence ppf ulam = match ulam with
  | Usequence(l1, l2) ->
      fprintf ppf "%a@ %a" sequence l1 sequence l2
  | _ -> lam ppf ulam

let clambda ppf ulam =
  fprintf ppf "%a@." lam ulam


let rec approx ppf = function
    Value_closure(fundesc, a) ->
      Format.fprintf ppf "@[<2>function %s@ arity %i"
        fundesc.fun_label fundesc.fun_arity;
      if fundesc.fun_closed then begin
        Format.fprintf ppf "@ (closed)"
      end;
      if fundesc.fun_inline <> None then begin
        Format.fprintf ppf "@ (inline)"
      end;
      Format.fprintf ppf "@ -> @ %a@]" approx a
  | Value_tuple a ->
      let tuple ppf a =
        for i = 0 to Array.length a - 1 do
          if i > 0 then Format.fprintf ppf ";@ ";
          Format.fprintf ppf "%i: %a" i approx a.(i)
        done in
      Format.fprintf ppf "@[<hov 1>(%a)@]" tuple a
  | Value_unknown ->
      Format.fprintf ppf "_"
  | Value_const c ->
      fprintf ppf "@[const(%a)@]" uconstant c
  | Value_global_field (s, i) ->
      fprintf ppf "@[global(%s,%i)@]" s i
