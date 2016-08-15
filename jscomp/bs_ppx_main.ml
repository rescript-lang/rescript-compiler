


let apply_lazy ~source ~target impl iface =
  let ic = open_in_bin source in
  let magic =
    really_input_string ic (String.length Config.ast_impl_magic_number)
  in
  if magic <> Config.ast_impl_magic_number
  && magic <> Config.ast_intf_magic_number then
    failwith "Ast_mapper: OCaml version mismatch or malformed input";
  Location.input_name := input_value ic;
  let ast = input_value ic in
  close_in ic;

  let ast =
    if magic = Config.ast_impl_magic_number
    then Obj.magic (impl (Obj.magic ast))
    else Obj.magic (iface (Obj.magic ast))
  in
  let oc = open_out_bin target in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc ast;
  close_out oc


let  () =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then
      apply_lazy ~source:a.(n - 2) ~target:a.(n - 1)
        !Ppx_entry.rewrite_implementation
        !Ppx_entry.rewrite_signature
    else begin
      Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
                     Sys.executable_name;
      exit 2
    end
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2

