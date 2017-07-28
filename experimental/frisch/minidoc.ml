open Asttypes
open Parsetree
open Typedtree
open Longident

let pendings = ref []

let doc ppf = function
  | ({txt="doc";_}, PStr [{pstr_desc=Pstr_eval(e, _); _}]) ->
      begin match e.pexp_desc with
      | Pexp_constant(Const_string (s, _)) ->
          Format.fprintf ppf "    --> %s@." s
      | Pexp_apply({pexp_desc=Pexp_ident{txt=Lident "section"}},
                   ["", {pexp_desc=Pexp_constant(Const_string (s, _))}]) ->
                     Format.fprintf ppf "  ==== %s ====@." s
      | _ -> ()
      end
  | _ -> ()

let rec signature path ppf sg =
  List.iter (signature_item path ppf) sg.sig_items

and signature_item path ppf si =
  match si.sig_desc with
  | Tsig_value x ->
      Format.fprintf ppf "  val %s: %a@." x.val_name.txt Printtyp.type_expr x.val_desc.ctyp_type;
      List.iter (doc ppf) x.val_attributes
  | Tsig_module x ->
      begin match x.md_type.mty_desc with
      | Tmty_ident (_, {txt=lid}) ->
          Format.fprintf ppf "  module %s: %a@." x.md_name.txt Printtyp.longident lid
      | Tmty_signature sg ->
          pendings := `Module (path ^ "." ^ x.md_name.txt, sg) :: !pendings;
          Format.fprintf ppf "  module %s: ... (see below)@." x.md_name.txt;
      | _ ->
          Format.fprintf ppf "  module %s: ...@." x.md_name.txt;
      end;
      List.iter (doc ppf) x.md_attributes
  | Tsig_type l ->
      List.iter (type_declaration ppf) l
  | Tsig_attribute x ->
      doc ppf x
  | _ ->
      ()

and type_declaration ppf x =
  Format.fprintf ppf "  type %s@." x.typ_name.txt;
  List.iter (doc ppf) x.typ_attributes

let component = function
  | `Module (path, sg) ->
      Format.printf "[[[ Interface for %s ]]]@.%a@."
        path (signature path) sg

let () =
  let open Cmt_format in
  for i = 1 to Array.length Sys.argv - 1 do
    let fn = Sys.argv.(i) in
    try
      let {cmt_annots; cmt_modname; _} = read_cmt fn in
      begin match cmt_annots with
      | Interface sg -> component (`Module (cmt_modname, sg))
      | _ -> ()
      end;
      while !pendings <> [] do
        let l = List.rev !pendings in
        pendings := [];
        List.iter component l
      done
    with exn ->
      Format.printf "Cannot read '%s': %s@." fn (Printexc.to_string exn)
  done
