let () =
  Clflags.parse ();
  if !Clflags.bench then (
    Benchmarks.run();
    exit 0;
  );
  if !Clflags.outcome then (
    Repl.typeAndPrintOutcome (List.hd !Clflags.files)
  ) else (
    let () = match !Clflags.files with
    | (file::_) as files ->
      List.iter (fun filename ->
        Driver.processFile
          ~isInterface:!Clflags.interface
          ~width:!Clflags.width
          ~recover:!Clflags.recover
          ~target:!Clflags.print
          ~origin:!Clflags.origin
          filename
      ) files;
    | [] ->
      Driver.processFile
        ~isInterface:!Clflags.interface
        ~width:!Clflags.width
        ~recover:!Clflags.recover
        ~target:!Clflags.print
        ~origin:!Clflags.origin
        ""
    in
    if !Clflags.profile then Profile.print();
    if !Clflags.bench then Benchmarks.run();
    exit 0
  )
