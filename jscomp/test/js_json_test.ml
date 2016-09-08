

let v = Js.Json.parse {| { "x" : [1, 2, 3 ] } |}


let () =
  let ty, x = Js.Json.reify_type v in
  match (ty : _ Js.Json.kind) with
  | Js.Json.Object ->  (* compiler infer x : Js.Json.t Js.Dict.t *) 
    begin match Js.Undefined.to_opt (Js.Dict.get x "x") with 
    | Some v -> 
      let ty2, x = Js.Json.reify_type v in
      begin match ty2 with 
      | Js.Json.Array ->  (* compiler infer x : Js.Json.t array *)
        x |> Js.Array.forEach (fun [@bs] x -> 
            let (ty3, x) = Js.Json.reify_type x in 
            match ty3 with 
            | Js.Json.Number -> Js.log (x +. 0.)
            | _ -> assert false
          )
      | _ -> assert false
      end
    | None -> 
      assert false 
    end
  | _ -> assert false


let () = 
  Js.log (Js.Json.test v Object)
