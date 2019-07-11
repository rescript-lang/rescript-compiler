let v = Liba.Demo.v

(* + Liba.Priv.v *)

let () =
  Js.log List.value ;
  (* local > stdlib *)
  Js.log Hello.src ;
  (* local > libb *)
  Js.log Map.value ;
  (* dep > stdlib *)
  Js.log Libb_module.value
