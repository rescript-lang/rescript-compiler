(* Js.Undefined.to_opt (if Js.typeof [%raw {|x|}] then [%raw{|x|}] else
   Js.Undefined.empty )*)
(* let a = match Js.Undefined.to_opt [%raw {|___undefined_value|}] with | None
   -> 1 | Some _ -> 2

   external file : string option = "__filename" [@@bs.val]
   [@@bs.return{undefined_to_opt}]

   let a = match file with | None -> 0 | Some x -> Js.log x ; 1

   (* let b = match [%external __filename] with | None -> 0 | Some x -> Js.log
   x ; 1 *)

   let c = let v = if Js.typeof [%raw {|__filename|} ] == "undefined" then
   [%raw {|__filename|} ] else Js.undefined in match Js.Undefined.to_opt v with
   | None -> 0 | Some _ -> 1 (* let f x = Js.Undefined.test x*) *)
