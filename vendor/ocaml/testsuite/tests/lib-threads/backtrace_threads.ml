
let () = Printexc.record_backtrace true

let () =
   let bt =
     try
       let h = (Hashtbl.create 1 : (int, unit) Hashtbl.t) in
       Hashtbl.find h 1;
       assert false
     with Not_found ->
       Printexc.get_raw_backtrace ()
   in
   let t = Thread.create (fun () ->
       try
         Printexc.raise_with_backtrace Not_found bt
       with Not_found -> ()
     ) () in
   Thread.join t;
   flush stdout
