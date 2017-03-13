
(* [@@@bs.config{no_export}] *)
module Fs = Node.Fs


let clean () = 
  match [%node __dirname] with
  | Some dir -> 
    let bin_dir = Node.Path.join [|dir ; ".."; "bin"|] in 
    let files = Fs.readdirSync bin_dir in 
    Js.log ("cleaning now", files);
    files |> Js.Array.forEach (fun  f -> 
        if (not @@ Js.String.startsWith "bs" f 
        && f <> ".gitignore" && f <> "ninja.exe")
        then 
          let p = Node.Path.join [| bin_dir; f|] in
          try 
            Js.log ("removing", p, "now");
            Fs.unlinkSync p 
          with _ -> Js.log ("removing", p, "failure")
      );
  | None -> assert false 




(* local variables: *)
(* compile-command: "bscc -c clean.ml" *)
(* end: *)
