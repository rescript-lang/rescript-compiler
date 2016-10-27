
(* [@@@bs.config{no_export}] *)
module Fs = Node.Fs


let clean () = 
  Js.Undefined.iter [%node __dirname] (fun [@bs] dir -> 
      let bin_dir = Node.Path.join [|dir ; ".."; "bin"|] in 
      let files = Fs.readdirSync bin_dir in 
      Js.log ("cleaning now", files);
      files |> Js.Array.forEach (fun [@bs] f -> 
          if not @@ Js.to_bool @@ Js.String.startsWith "bs" f 
             && f <> ".gitignore"
          then 
            let p = Node.Path.join [| bin_dir; f|] in
            try 
              Js.log ("removing", p, "now");
              Fs.unlinkSync p 
            with _ -> Js.log ("removing", p, "failure")
        );

    )



(* local variables: *)
(* compile-command: "bscc -c clean.ml" *)
(* end: *)
