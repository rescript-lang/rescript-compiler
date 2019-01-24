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

type value_string_contents =
  | Contents of string
  | Unknown_or_mutable

type value_string = {
  contents : value_string_contents;
  size : int;
}

type value_float_array_contents =
  | Contents of float option array
  | Unknown_or_mutable

type value_float_array = {
  contents : value_float_array_contents;
  size : int;
}

type descr =
  | Value_block of Tag.t * approx array
  | Value_mutable_block of Tag.t * int
  | Value_int of int
  | Value_char of char
  | Value_constptr of int
  | Value_float of float
  | Value_float_array of value_float_array
  | Value_boxed_int : 'a Simple_value_approx.boxed_int * 'a -> descr
  | Value_string of value_string
  | Value_closure of value_closure
  | Value_set_of_closures of value_set_of_closures

and value_closure = {
  closure_id : Closure_id.t;
  set_of_closures : value_set_of_closures;
}

and value_set_of_closures = {
  set_of_closures_id : Set_of_closures_id.t;
  bound_vars : approx Var_within_closure.Map.t;
  results : approx Closure_id.Map.t;
  aliased_symbol : Symbol.t option;
}

and approx =
  | Value_unknown
  | Value_id of Export_id.t
  | Value_symbol of Symbol.t

let equal_approx (a1:approx) (a2:approx) =
  match a1, a2 with
  | Value_unknown, Value_unknown ->
    true
  | Value_id id1, Value_id id2 ->
    Export_id.equal id1 id2
  | Value_symbol s1, Value_symbol s2 ->
    Symbol.equal s1 s2
  | (Value_unknown | Value_symbol _ | Value_id _),
    (Value_unknown | Value_symbol _ | Value_id _) ->
    false

let equal_array eq a1 a2 =
  Array.length a1 = Array.length a2 &&
  try
    Array.iteri (fun i v1 -> if not (eq a2.(i) v1) then raise Exit) a1;
    true
  with Exit -> false

let equal_option eq o1 o2 =
  match o1, o2 with
  | None, None -> true
  | Some v1, Some v2 -> eq v1 v2
  | Some _, None | None, Some _ -> false

let equal_set_of_closures (s1:value_set_of_closures)
      (s2:value_set_of_closures) =
  Set_of_closures_id.equal s1.set_of_closures_id s2.set_of_closures_id &&
  Var_within_closure.Map.equal equal_approx s1.bound_vars s2.bound_vars &&
  Closure_id.Map.equal equal_approx s1.results s2.results &&
  equal_option Symbol.equal s1.aliased_symbol s2.aliased_symbol

let equal_descr (d1:descr) (d2:descr) : bool =
  match d1, d2 with
  | Value_block (t1, f1), Value_block (t2, f2) ->
    Tag.equal t1 t2 && equal_array equal_approx f1 f2
  | Value_mutable_block (t1, s1), Value_mutable_block (t2, s2) ->
    Tag.equal t1 t2 &&
    s1 = s2
  | Value_int i1, Value_int i2 ->
    i1 = i2
  | Value_char c1, Value_char c2 ->
    c1 = c2
  | Value_constptr i1, Value_constptr i2 ->
    i1 = i2
  | Value_float f1, Value_float f2 ->
    f1 = f2
  | Value_float_array s1, Value_float_array s2 ->
    s1 = s2
  | Value_boxed_int (t1, v1), Value_boxed_int (t2, v2) ->
    Simple_value_approx.equal_boxed_int t1 v1 t2 v2
  | Value_string s1, Value_string s2 ->
    s1 = s2
  | Value_closure c1, Value_closure c2 ->
    Closure_id.equal c1.closure_id c2.closure_id &&
    equal_set_of_closures c1.set_of_closures c2.set_of_closures
  | Value_set_of_closures s1, Value_set_of_closures s2 ->
    equal_set_of_closures s1 s2
  | ( Value_block (_, _) | Value_mutable_block (_, _) | Value_int _
    | Value_char _ | Value_constptr _ | Value_float _ | Value_float_array _
    | Value_boxed_int _ | Value_string _ | Value_closure _
    | Value_set_of_closures _ ),
    ( Value_block (_, _) | Value_mutable_block (_, _) | Value_int _
    | Value_char _ | Value_constptr _ | Value_float _ | Value_float_array _
    | Value_boxed_int _ | Value_string _ | Value_closure _
    | Value_set_of_closures _ ) ->
    false

type t = {
  sets_of_closures : Flambda.function_declarations Set_of_closures_id.Map.t;
  closures : Flambda.function_declarations Closure_id.Map.t;
  values : descr Export_id.Map.t Compilation_unit.Map.t;
  symbol_id : Export_id.t Symbol.Map.t;
  offset_fun : int Closure_id.Map.t;
  offset_fv : int Var_within_closure.Map.t;
  constant_sets_of_closures : Set_of_closures_id.Set.t;
  invariant_params : Variable.Set.t Variable.Map.t Set_of_closures_id.Map.t;
}

let empty : t = {
  sets_of_closures = Set_of_closures_id.Map.empty;
  closures = Closure_id.Map.empty;
  values = Compilation_unit.Map.empty;
  symbol_id = Symbol.Map.empty;
  offset_fun = Closure_id.Map.empty;
  offset_fv = Var_within_closure.Map.empty;
  constant_sets_of_closures = Set_of_closures_id.Set.empty;
  invariant_params = Set_of_closures_id.Map.empty;
}

let create ~sets_of_closures ~closures ~values ~symbol_id
      ~offset_fun ~offset_fv ~constant_sets_of_closures
      ~invariant_params =
  { sets_of_closures;
    closures;
    values;
    symbol_id;
    offset_fun;
    offset_fv;
    constant_sets_of_closures;
    invariant_params;
  }

let add_clambda_info t ~offset_fun ~offset_fv ~constant_sets_of_closures =
  assert (Closure_id.Map.cardinal t.offset_fun = 0);
  assert (Var_within_closure.Map.cardinal t.offset_fv = 0);
  assert (Set_of_closures_id.Set.cardinal t.constant_sets_of_closures = 0);
  { t with offset_fun; offset_fv; constant_sets_of_closures; }

let merge (t1 : t) (t2 : t) : t =
  let eidmap_disjoint_union ?eq map1 map2 =
    Compilation_unit.Map.merge (fun _id map1 map2 ->
        match map1, map2 with
        | None, None -> None
        | None, Some map
        | Some map, None -> Some map
        | Some map1, Some map2 ->
          Some (Export_id.Map.disjoint_union ?eq map1 map2))
      map1 map2
  in
  let int_eq (i : int) j = i = j in
  { values = eidmap_disjoint_union ~eq:equal_descr t1.values t2.values;
    sets_of_closures =
      Set_of_closures_id.Map.disjoint_union t1.sets_of_closures
        t2.sets_of_closures;
    closures = Closure_id.Map.disjoint_union t1.closures t2.closures;
    symbol_id = Symbol.Map.disjoint_union ~print:Export_id.print t1.symbol_id t2.symbol_id;
    offset_fun = Closure_id.Map.disjoint_union
        ~eq:int_eq t1.offset_fun t2.offset_fun;
    offset_fv = Var_within_closure.Map.disjoint_union
        ~eq:int_eq t1.offset_fv t2.offset_fv;
    constant_sets_of_closures =
      Set_of_closures_id.Set.union t1.constant_sets_of_closures
        t2.constant_sets_of_closures;
    invariant_params =
      Set_of_closures_id.Map.disjoint_union
        ~print:(Variable.Map.print Variable.Set.print)
        ~eq:(Variable.Map.equal Variable.Set.equal)
        t1.invariant_params t2.invariant_params;
  }

let find_value eid map =
  let unit_map =
    Compilation_unit.Map.find (Export_id.get_compilation_unit eid) map
  in
  Export_id.Map.find eid unit_map

let find_description (t : t) eid =
  find_value eid t.values

let nest_eid_map map =
  let add_map eid v map =
    let unit = Export_id.get_compilation_unit eid in
    let m =
      try Compilation_unit.Map.find unit map
      with Not_found -> Export_id.Map.empty
    in
    Compilation_unit.Map.add unit (Export_id.Map.add eid v m) map
  in
  Export_id.Map.fold add_map map Compilation_unit.Map.empty

let print_approx ppf ((t,root_symbols) : t * Symbol.t list) =
  let values = t.values in
  let fprintf = Format.fprintf in
  let printed = ref Export_id.Set.empty in
  let recorded_symbol = ref Symbol.Set.empty in
  let symbols_to_print = Queue.create () in
  let printed_set_of_closures = ref Set_of_closures_id.Set.empty in
  let rec print_approx ppf (approx : approx) =
    match approx with
    | Value_unknown -> fprintf ppf "?"
    | Value_id id ->
      if Export_id.Set.mem id !printed then
        fprintf ppf "(%a: _)" Export_id.print id
      else begin
        try
          let descr = find_value id values in
          printed := Export_id.Set.add id !printed;
          fprintf ppf "@[<hov 2>(%a:@ %a)@]"
            Export_id.print id print_descr descr
        with Not_found ->
          fprintf ppf "(%a: Not available)" Export_id.print id
      end
    | Value_symbol sym ->
      if not (Symbol.Set.mem sym !recorded_symbol) then begin
        recorded_symbol := Symbol.Set.add sym !recorded_symbol;
        Queue.push sym symbols_to_print;
      end;
      Symbol.print ppf sym
  and print_descr ppf (descr : descr) =
    match descr with
    | Value_int i -> Format.pp_print_int ppf i
    | Value_char c -> fprintf ppf "%c" c
    | Value_constptr i -> fprintf ppf "%ip" i
    | Value_block (tag, fields) ->
      fprintf ppf "[%a:%a]" Tag.print tag print_fields fields
    | Value_mutable_block (tag, size) ->
      fprintf ppf "[mutable %a:%i]" Tag.print tag size
    | Value_closure {closure_id; set_of_closures} ->
      fprintf ppf "(closure %a, %a)" Closure_id.print closure_id
        print_set_of_closures set_of_closures
    | Value_set_of_closures set_of_closures ->
      fprintf ppf "(set_of_closures %a)" print_set_of_closures set_of_closures
    | Value_string { contents; size } ->
      begin match contents with
      | Unknown_or_mutable -> Format.fprintf ppf "string %i" size
      | Contents s ->
        let s =
          if size > 10
          then String.sub s 0 8 ^ "..."
          else s
        in
        Format.fprintf ppf "string %i %S" size s
      end
    | Value_float f -> Format.pp_print_float ppf f
    | Value_float_array float_array ->
      Format.fprintf ppf "float_array%s %i"
        (match float_array.contents with
          | Unknown_or_mutable -> ""
          | Contents _ -> "_imm")
        float_array.size
    | Value_boxed_int (t, i) ->
      let module A = Simple_value_approx in
      match t with
      | A.Int32 -> Format.fprintf ppf "%li" i
      | A.Int64 -> Format.fprintf ppf "%Li" i
      | A.Nativeint -> Format.fprintf ppf "%ni" i
  and print_fields ppf fields =
    Array.iter (fun approx -> fprintf ppf "%a@ " print_approx approx) fields
  and print_set_of_closures ppf
      { set_of_closures_id; bound_vars; aliased_symbol; results } =
    if Set_of_closures_id.Set.mem set_of_closures_id !printed_set_of_closures
    then fprintf ppf "%a" Set_of_closures_id.print set_of_closures_id
    else begin
      printed_set_of_closures :=
        Set_of_closures_id.Set.add set_of_closures_id !printed_set_of_closures;
      let print_alias ppf = function
        | None -> ()
        | Some symbol ->
          Format.fprintf ppf "@ (alias: %a)" Symbol.print symbol
      in
      fprintf ppf "{%a: %a%a => %a}"
        Set_of_closures_id.print set_of_closures_id
        print_binding bound_vars
        print_alias aliased_symbol
        (Closure_id.Map.print print_approx) results
    end
  and print_binding ppf bound_vars =
    Var_within_closure.Map.iter (fun clos_id approx ->
        fprintf ppf "%a -> %a,@ "
          Var_within_closure.print clos_id
          print_approx approx)
      bound_vars
  in
  let rec print_recorded_symbols () =
    if not (Queue.is_empty symbols_to_print) then begin
      let sym = Queue.pop symbols_to_print in
      begin match Symbol.Map.find sym t.symbol_id with
      | exception Not_found -> ()
      | id ->
        fprintf ppf "@[<hov 2>%a:@ %a@];@ "
          Symbol.print sym
          print_approx (Value_id id)
      end;
      print_recorded_symbols ();
    end
  in
  List.iter (fun s -> Queue.push s symbols_to_print) root_symbols;
  fprintf ppf "@[<hov 2>Globals:@ ";
  fprintf ppf "@]@ @[<hov 2>Symbols:@ ";
  print_recorded_symbols ();
  fprintf ppf "@]"

let print_offsets ppf (t : t) =
  Format.fprintf ppf "@[<v 2>offset_fun:@ ";
  Closure_id.Map.iter (fun cid off ->
      Format.fprintf ppf "%a -> %i@ "
        Closure_id.print cid off) t.offset_fun;
  Format.fprintf ppf "@]@ @[<v 2>offset_fv:@ ";
  Var_within_closure.Map.iter (fun vid off ->
      Format.fprintf ppf "%a -> %i@ "
        Var_within_closure.print vid off) t.offset_fv;
  Format.fprintf ppf "@]@ "

let print_functions ppf (t : t) =
  Set_of_closures_id.Map.print Flambda.print_function_declarations ppf
    t.sets_of_closures

let print_all ppf ((t, root_symbols) : t * Symbol.t list) =
  let fprintf = Format.fprintf in
  fprintf ppf "approxs@ %a@.@."
    print_approx (t, root_symbols);
  fprintf ppf "functions@ %a@.@."
    print_functions t
