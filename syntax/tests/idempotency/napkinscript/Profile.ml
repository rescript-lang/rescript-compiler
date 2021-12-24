module Profile: sig
  val record : name:string -> (unit -> 'a) -> 'a
  val print: unit -> unit
end = struct
  let state = Hashtbl.create 2

  let record ~name f =
    let startTime = Time.now() in
    let result = f() in
    let endTime = Time.now() in

    Hashtbl.add state name (Time.diff startTime endTime);
    result

  let print () =
    let report = Hashtbl.fold (fun k v acc ->
      let line = Printf.sprintf "%s: %fms\n" k (Time.print v) in
      acc ^ line
    ) state "\n\n"
    in
    print_endline report
end
