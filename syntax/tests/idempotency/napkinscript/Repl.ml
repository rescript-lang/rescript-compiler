module Repl = struct
  let parseToplevelPhrase filename =
    let src = IO.readFile filename in
    let p = Parser.make src filename in
    Parsetree.Ptop_def (NapkinScript.parseImplementation p)


  let typeAndPrintOutcome filename =
    Compmisc.init_path false;
    let env = Compmisc.initial_env () in
    try
      let sstr = match parseToplevelPhrase filename with
      | Parsetree.Ptop_def sstr -> sstr
      | _ -> assert false
      in
      let (_str, signature, newenv) = Typemod.type_toplevel_phrase env sstr in
      let outSigItems = Printtyp.tree_of_signature signature in
      let fmt = Format.str_formatter in
      !OutcomePrinter.out_signature fmt outSigItems;
      let result = Format.flush_str_formatter () in
      print_string result
    with
    | Typetexp.Error (_, _, err) ->
      let fmt = Format.str_formatter in
      Typetexp.report_error env fmt err;
      let result = Format.flush_str_formatter () in
      let () = print_endline result in
      ()
    | _ -> print_endline "catch all"

end
