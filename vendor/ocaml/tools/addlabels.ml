(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the Q Public License   *)
(*  version 1.0.                                                       *)
(*                                                                     *)
(***********************************************************************)

open StdLabels
open Asttypes
open Parsetree

let norec = ref false

let input_file file =
  let ic = try open_in file with _ -> failwith ("input_file : " ^ file) in
  let b = Buffer.create 1024 in
  let buf = String.create 1024 and len = ref 0 in
  while len := input ic buf 0 1024; !len > 0 do
    Buffer.add_substring b buf 0 !len
  done;
  close_in ic;
  Buffer.contents b

module SMap = struct
  include Map.Make(struct type t = string let compare = compare end)
  let rec removes l m =
    match l with [] -> m
    | k::l ->
        let m = try remove k m with Not_found -> m in
        removes l m
end

let rec labels_of_sty sty =
  match sty.ptyp_desc with
    Ptyp_arrow (lab, _, rem) -> lab :: labels_of_sty rem
  | Ptyp_alias (rem, _)      -> labels_of_sty rem
  |  _                       -> []

let rec labels_of_cty cty =
  match cty.pcty_desc with
    Pcty_arrow (lab, _, rem) ->
      let (labs, meths) = labels_of_cty rem in
      (lab :: labs, meths)
  | Pcty_signature { pcsig_fields = fields } ->
      ([],
       List.fold_left fields ~init:[] ~f:
          begin fun meths -> function
          { pctf_desc = Pctf_meth (s, _, sty) } -> (s, labels_of_sty sty)::meths
            | _ -> meths
          end)
  |  _ ->
      ([],[])

let rec pattern_vars pat =
  match pat.ppat_desc with
    Ppat_var s -> [s.txt]
  | Ppat_alias (pat, s) ->
      s.txt :: pattern_vars pat
  | Ppat_tuple l
  | Ppat_array l ->
      List.concat (List.map pattern_vars l)
  | Ppat_construct (_, Some pat)
  | Ppat_variant (_, Some pat)
  | Ppat_constraint (pat, _) ->
      pattern_vars pat
  | Ppat_record(l, _) ->
      List.concat (List.map l ~f:(fun (_,p) -> pattern_vars p))
  | Ppat_or (pat1, pat2) ->
      pattern_vars pat1 @ pattern_vars pat2
  | Ppat_lazy pat -> pattern_vars pat
  | Ppat_any | Ppat_constant _ | Ppat_construct _ | Ppat_variant _
  | Ppat_type _ | Ppat_unpack _ ->
      []

let pattern_name pat =
  match pat.ppat_desc with
    Ppat_var s -> Some s
  | Ppat_constraint ({ppat_desc = Ppat_var s}, _) -> Some s
  | _ -> None

let insertions = ref []
let add_insertion pos s = insertions := (pos,s) :: !insertions
let sort_insertions () =
  List.sort !insertions ~cmp:(fun (pos1,_) (pos2,_) -> pos1 - pos2)

let is_space = function ' '|'\t'|'\n'|'\r' -> true | _ -> false
let is_alphanum = function 'A'..'Z'|'a'..'z'|'_'|'\192'..'\214'|'\216'..'\246'
  | '\248'..'\255'|'\''|'0'..'9' -> true
  | _ -> false

(* Remove "(" or "begin" before a pattern *)
let rec insertion_point pos ~text =
  let pos' = ref (pos-1) in
  while is_space text.[!pos'] do decr pos' done;
  if text.[!pos'] = '(' then insertion_point !pos' ~text else
  if !pos' >= 5 && String.sub text ~pos:(!pos'-4) ~len:5 = "begin"
  && not (is_alphanum text.[!pos'-5]) then insertion_point (!pos'-4) ~text
  else pos

(* Search "=" or "->" before "function" *)
let rec insertion_point2 pos ~text =
  let pos' = ref (pos-1) in
  while is_space text.[!pos'] do decr pos' done;
  if text.[!pos'] = '(' then insertion_point2 !pos' ~text else
  if !pos' >= 5 && String.sub text ~pos:(!pos'-4) ~len:5 = "begin"
  && not (is_alphanum text.[!pos'-5]) then insertion_point2 (!pos'-4) ~text
  else if text.[!pos'] = '=' then Some !pos' else
  if !pos' >= 1 && text.[!pos'-1] = '-' && text.[!pos'] = '>'
  then Some (!pos' - 1)
  else None

let rec insert_labels ~labels ~text expr =
  match labels, expr.pexp_desc with
    l::labels, Pexp_function(l', _, [pat, rem]) ->
      if l <> "" && l.[0] <> '?' && l' = "" then begin
        let start_c = pat.ppat_loc.Location.loc_start.Lexing.pos_cnum in
        let pos = insertion_point start_c ~text in
        match pattern_name pat with
        | Some name when l = name.txt -> add_insertion pos "~"
        | _ -> add_insertion pos ("~" ^ l ^ ":")
      end;
      insert_labels ~labels ~text rem
  | l::labels, Pexp_function(l', _, lst) ->
      let pos = expr.pexp_loc.Location.loc_start.Lexing.pos_cnum in
      if l <> "" && l.[0] <> '?' && l' = ""
      && String.sub text ~pos ~len:8 = "function" then begin
        String.blit ~src:"match th" ~src_pos:0 ~dst:text
          ~dst_pos:pos ~len:8;
        add_insertion (pos+6) (l ^ " wi");
        match insertion_point2 pos ~text with
          Some pos' ->
            add_insertion pos' ("~" ^ l ^ " ")
        | None ->
            add_insertion pos ("fun ~" ^ l ^ " -> ")
      end;
      List.iter lst ~f:(fun (p,e) -> insert_labels ~labels ~text e)
  | _, Pexp_match( _, lst) ->
      List.iter lst ~f:(fun (p,e) -> insert_labels ~labels ~text e)
  | _, Pexp_try(expr, lst) ->
      insert_labels ~labels ~text expr;
      List.iter lst ~f:(fun (p,e) -> insert_labels ~labels ~text e)
  | _, ( Pexp_let(_,_,e) | Pexp_sequence(_,e) | Pexp_when(_,e)
       | Pexp_constraint(e,_,_) | Pexp_letmodule(_,_,e)
       | Pexp_ifthenelse(_,e,None) ) ->
      insert_labels ~labels ~text e
  | _, Pexp_ifthenelse (_, e1, Some e2) ->
      insert_labels ~labels ~text e1;
      insert_labels ~labels ~text e2
  | _ ->
      ()

let rec insert_labels_class ~labels ~text expr =
  match labels, expr.pcl_desc with
    l::labels, Pcl_fun(l', _, pat, rem) ->
      if l <> "" && l.[0] <> '?' && l' = "" then begin
        let start_c = pat.ppat_loc.Location.loc_start.Lexing.pos_cnum in
        let pos = insertion_point start_c ~text in
        match pattern_name pat with
        | Some name when l = name.txt -> add_insertion pos "~"
        | _ -> add_insertion pos ("~" ^ l ^ ":")
      end;
      insert_labels_class ~labels ~text rem
  | labels, (Pcl_constraint (expr, _) | Pcl_let (_, _, expr)) ->
      insert_labels_class ~labels ~text expr
  | _ ->
      ()

let rec insert_labels_type ~labels ~text ty =
  match labels, ty.ptyp_desc with
    l::labels, Ptyp_arrow(l', _, rem) ->
      if l <> "" && l.[0] <> '?' && l' = "" then begin
        let start_c = ty.ptyp_loc.Location.loc_start.Lexing.pos_cnum in
        let pos = insertion_point start_c ~text in
        add_insertion pos (l ^ ":")
      end;
      insert_labels_type ~labels ~text rem
  | _ ->
      ()

let rec insert_labels_app ~labels ~text args =
  match labels, args with
    l::labels, (l',arg)::args ->
      if l <> "" && l.[0] <> '?' && l' = "" then begin
        let pos0 = arg.pexp_loc.Location.loc_start.Lexing.pos_cnum in
        let pos = insertion_point pos0 ~text in
        match arg.pexp_desc with
        | Pexp_ident({ txt = Longident.Lident name })
          when l = name && pos = pos0 ->
            add_insertion pos "~"
        | _ -> add_insertion pos ("~" ^ l ^ ":")
      end;
      insert_labels_app ~labels ~text args
  | _ ->
      ()

let insert_labels_app ~labels ~text args =
  let labels, opt_labels =
    List.partition labels ~f:(fun l -> l = "" || l.[0] <> '?') in
  let nopt_labels =
    List.map opt_labels
      ~f:(fun l -> String.sub l ~pos:1 ~len:(String.length l - 1)) in
  (* avoid ambiguous labels *)
  if List.exists labels ~f:(List.mem ~set:nopt_labels) then () else
  let aopt_labels = opt_labels @ nopt_labels in
  let args, lab_args = List.partition args ~f:(fun (l,_) -> l = "") in
  (* only optional arguments are labeled *)
  if List.for_all lab_args ~f:(fun (l,_) -> List.mem l ~set:aopt_labels)
  then insert_labels_app ~labels ~text args

let rec add_labels_expr ~text ~values ~classes expr =
  let add_labels_rec ?(values=values) expr =
    add_labels_expr ~text ~values ~classes expr in
  match expr.pexp_desc with
    Pexp_apply ({pexp_desc=Pexp_ident({ txt = Longident.Lident s })}, args) ->
      begin try
        let labels = SMap.find s values in
        insert_labels_app ~labels ~text args
      with Not_found -> ()
      end;
      List.iter args ~f:(fun (_,e) -> add_labels_rec e)
  | Pexp_apply ({pexp_desc=Pexp_send
                   ({pexp_desc=Pexp_ident({ txt = Longident.Lident s })},
                    meth)},
                args) ->
      begin try
        if SMap.find s values = ["<object>"] then
          let labels = SMap.find (s ^ "#" ^ meth) values in
          insert_labels_app ~labels ~text args
      with Not_found -> ()
      end
  | Pexp_apply ({pexp_desc=Pexp_new ({ txt = Longident.Lident s })}, args) ->
      begin try
        let labels = SMap.find s classes in
        insert_labels_app ~labels ~text args
      with Not_found -> ()
      end
  | Pexp_let (recp, lst, expr) ->
      let vars = List.concat (List.map lst ~f:(fun (p,_) -> pattern_vars p)) in
      let vals = SMap.removes vars values in
      List.iter lst ~f:
        begin fun (_,e) ->
          add_labels_rec e ~values:(if recp = Recursive then vals else values)
        end;
      add_labels_rec expr ~values:vals
  | Pexp_function (_, None, lst) ->
      List.iter lst ~f:
        (fun (p,e) ->
          add_labels_rec e ~values:(SMap.removes (pattern_vars p) values))
  | Pexp_function (_, Some e, lst)
  | Pexp_match (e, lst)
  | Pexp_try (e, lst) ->
      add_labels_rec e;
      List.iter lst ~f:
        (fun (p,e) ->
          add_labels_rec e ~values:(SMap.removes (pattern_vars p) values))
  | Pexp_apply (e, args) ->
      List.iter add_labels_rec (e :: List.map snd args)
  | Pexp_tuple l | Pexp_array l ->
      List.iter add_labels_rec l
  | Pexp_construct (_, Some e)
  | Pexp_variant (_, Some e)
  | Pexp_field (e, _)
  | Pexp_constraint (e, _, _)
  | Pexp_send (e, _)
  | Pexp_setinstvar (_, e)
  | Pexp_letmodule (_, _, e)
  | Pexp_assert e
  | Pexp_lazy e
  | Pexp_poly (e, _)
  | Pexp_newtype (_, e)
  | Pexp_open (_, e) ->
      add_labels_rec e
  | Pexp_record (lst, opt) ->
      List.iter lst ~f:(fun (_,e) -> add_labels_rec e);
      begin match opt with Some e -> add_labels_rec e | None -> () end
  | Pexp_setfield (e1, _, e2)
  | Pexp_ifthenelse (e1, e2, None)
  | Pexp_sequence (e1, e2)
  | Pexp_while (e1, e2)
  | Pexp_when (e1, e2) ->
      add_labels_rec e1; add_labels_rec e2
  | Pexp_ifthenelse (e1, e2, Some e3) ->
      add_labels_rec e1; add_labels_rec e2; add_labels_rec e3
  | Pexp_for (s, e1, e2, _, e3) ->
      add_labels_rec e1; add_labels_rec e2;
      add_labels_rec e3 ~values:(SMap.removes [s.txt] values)
  | Pexp_override lst ->
      List.iter lst ~f:(fun (_,e) -> add_labels_rec e)
  | Pexp_ident _ | Pexp_constant _ | Pexp_construct _ | Pexp_variant _
  | Pexp_new _ | Pexp_object _ | Pexp_pack _ ->
      ()

let rec add_labels_class ~text ~classes ~values ~methods cl =
  match cl.pcl_desc with
    Pcl_constr _ -> ()
  | Pcl_structure { pcstr_self = p; pcstr_fields = l } ->
      let values = SMap.removes (pattern_vars p) values in
      let values =
        match pattern_name p with None -> values
        | Some s ->
            List.fold_left methods
              ~init:(SMap.add s.txt ["<object>"] values)
              ~f:(fun m (k,l) -> SMap.add (s.txt^"#"^k) l m)
      in
      ignore (List.fold_left l ~init:values ~f:
        begin fun values -> function e -> match e.pcf_desc with
          | Pcf_val (s, _, _, e) ->
              add_labels_expr ~text ~classes ~values e;
              SMap.removes [s.txt] values
          | Pcf_meth (s, _, _, e) ->
              begin try
                let labels = List.assoc s.txt methods in
                insert_labels ~labels ~text e
              with Not_found -> ()
              end;
              add_labels_expr ~text ~classes ~values e;
              values
          | Pcf_init e ->
              add_labels_expr ~text ~classes ~values e;
              values
          | Pcf_inher _ | Pcf_valvirt _ | Pcf_virt _ | Pcf_constr _ -> values
        end)
  | Pcl_fun (_, opt, pat, cl) ->
      begin match opt with None -> ()
      | Some e -> add_labels_expr ~text ~classes ~values e
      end;
      let values = SMap.removes (pattern_vars pat) values in
      add_labels_class ~text ~classes ~values ~methods cl
  | Pcl_apply (cl, args) ->
      List.iter args ~f:(fun (_,e) -> add_labels_expr ~text ~classes ~values e);
      add_labels_class ~text ~classes ~values ~methods cl
  | Pcl_let (recp, lst, cl) ->
      let vars = List.concat (List.map lst ~f:(fun (p,_) -> pattern_vars p)) in
      let vals = SMap.removes vars values in
      List.iter lst ~f:
        begin fun (_,e) ->
          add_labels_expr e ~text ~classes
            ~values:(if recp = Recursive then vals else values)
        end;
      add_labels_class cl ~text ~classes ~values:vals ~methods
  | Pcl_constraint (cl, _) ->
      add_labels_class ~text ~classes ~values ~methods cl

let add_labels ~intf ~impl ~file =
  insertions := [];
  let values, classes =
    List.fold_left intf ~init:(SMap.empty, SMap.empty) ~f:
      begin fun (values, classes as acc) item ->
        match item.psig_desc with
          Psig_value (name, {pval_type = sty}) ->
            (SMap.add name.txt (labels_of_sty sty) values, classes)
        | Psig_class l ->
          (values,
           List.fold_left l ~init:classes ~f:
             begin fun classes {pci_name=name; pci_expr=cty} ->
               SMap.add name.txt (labels_of_cty cty) classes
             end)
        | _ ->
            acc
      end
  in
  let text = input_file file in
  ignore (List.fold_right impl ~init:(values, classes) ~f:
    begin fun item (values, classes as acc) ->
      match item.pstr_desc with
        Pstr_value (recp, l) ->
          let names =
            List.concat (List.map l ~f:(fun (p,_) -> pattern_vars p)) in
          List.iter l ~f:
            begin fun (pat, expr) ->
              begin match pattern_name pat with
              | Some s ->
                  begin try
                    let labels = SMap.find s.txt values in
                    insert_labels ~labels ~text expr;
                    if !norec then () else
                    let values =
                      SMap.fold
                        (fun s l m ->
                          if List.mem s names then SMap.add s l m else m)
                        values SMap.empty in
                    add_labels_expr expr ~text ~values ~classes:SMap.empty
                  with Not_found -> ()
                  end
              | None -> ()
              end;
            end;
          (SMap.removes names values, classes)
      | Pstr_primitive (s, {pval_type=sty}) ->
          begin try
            let labels = SMap.find s.txt values in
            insert_labels_type ~labels ~text sty;
            (SMap.removes [s.txt] values, classes)
          with Not_found -> acc
          end
      | Pstr_class l ->
          let names = List.map l ~f:(fun pci -> pci.pci_name.txt) in
          List.iter l ~f:
            begin fun {pci_name=name; pci_expr=expr} ->
              try
                let (labels, methods) = SMap.find name.txt classes in
                insert_labels_class ~labels ~text expr;
                if !norec then () else
                let classes =
                  SMap.fold
                    (fun s (l,_) m ->
                      if List.mem s names then SMap.add s l m else m)
                    classes SMap.empty in
                add_labels_class expr ~text ~classes ~methods
                  ~values:SMap.empty
              with Not_found -> ()
            end;
          (values, SMap.removes names classes)
      | _ ->
          acc
    end);
  if !insertions <> [] then begin
    let backup = file ^ ".bak" in
    if Sys.file_exists backup then Sys.remove file
    else Sys.rename file backup;
    let oc = open_out file in
    let last_pos =
      List.fold_left (sort_insertions ()) ~init:0 ~f:
        begin fun pos (pos', s) ->
          output oc text pos (pos'-pos);
          output_string oc s;
          pos'
        end in
    if last_pos < String.length text then
      output oc text last_pos (String.length text - last_pos);
    close_out oc
  end
  else prerr_endline ("No labels to insert in " ^ file)

let process_file file =
  prerr_endline ("Processing " ^ file);
  if Filename.check_suffix file ".ml" then
    let intf = Filename.chop_suffix file ".ml" ^ ".mli" in
    let ic = open_in intf in
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf intf;
    let intf = Parse.interface lexbuf in
    close_in ic;
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf file;
    let impl = Parse.implementation lexbuf in
    close_in ic;
    add_labels ~intf ~impl ~file
  else prerr_endline (file ^ " is not an implementation")

let main () =
  let files = ref [] in
  Arg.parse ["-norec", Arg.Set norec, "do not labelize recursive calls"]
    (fun f -> files := f :: !files)
    "addlabels [-norec] <files>";
  let files = List.rev !files in
  List.iter files ~f:process_file

let () = main ()
