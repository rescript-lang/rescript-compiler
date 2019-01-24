let () =
  let cmi = Cmi_format.read_cmi "foo.cmi" in
  let data = Marshal.to_string cmi [] in
  Printf.printf "let foo = %S\n" data
