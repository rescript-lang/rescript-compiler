(* This tool reports values exported by .mli files but never used in any other module.
   It assumes that .mli files are compiled with -keep-locs and .ml files with -bin-annot.
   This can be enforced by setting:

      OCAMLPARAM=bin-annot=1,keep-locs=1,_
*)


open Types
open Typedtree

let vds = ref []  (* all exported value declarations *)
let references = Hashtbl.create 256  (* all value references *)

let unit fn =
  Filename.chop_extension (Filename.basename fn)

let rec collect_export fn = function
  | Sig_value (_, {Types.val_loc; _}) when not val_loc.Location.loc_ghost ->
      (* a .cmi file can contain locations from other files.
         For instance:
             module M : Set.S with type elt = int
         will create value definitions whole locations is in set.mli
      *)
      if unit fn = unit val_loc.Location.loc_start.Lexing.pos_fname then
        vds := val_loc :: !vds
  | Sig_module (_, {Types.md_type=Mty_signature sg; _}, _) -> List.iter (collect_export fn) sg
  | _ -> ()

let collect_references = object
  inherit Tast_iter.iter as super
  method! expression = function
    | {exp_desc = Texp_ident (_, _, {Types.val_loc; _}); exp_loc} -> Hashtbl.add references val_loc exp_loc
    | e -> super # expression e
end

let rec load_file fn =
  if Filename.check_suffix fn ".cmi"
      && Sys.file_exists (Filename.chop_suffix fn ".cmi" ^ ".mli") then
    (* only consider module with an explicit interface *)
    let open Cmi_format in
(*    Printf.eprintf "Scanning %s\n%!" fn; *)
    List.iter (collect_export fn) (read_cmi fn).cmi_sign
  else if Filename.check_suffix fn ".cmt" then
    let open Cmt_format in
(*    Printf.eprintf "Scanning %s\n%!" fn; *)
    match read fn with
    | (_, Some {cmt_annots = Implementation x; _}) -> collect_references # structure x
    | _ -> ()  (* todo: support partial_implementation? *)
  else if (try Sys.is_directory fn with _ -> false) then
    Array.iter (fun s -> load_file (Filename.concat fn s)) (Sys.readdir fn)

let report loc =
  if not (Hashtbl.mem references loc) then
    Format.printf "%a: unused exported value@." Location.print_loc loc

let () =
  try
    for i = 1 to Array.length Sys.argv - 1 do load_file Sys.argv.(i) done;
    List.iter report !vds
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
