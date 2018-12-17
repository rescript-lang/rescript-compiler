let direct : int list ref = ref []
let indirect : int list ref = ref []

#if 0 then (*FIXME*)
let () =
  for i = 0 to 3 do
    let rec f = function
      | 0 -> i
      | -1 -> g (-2) (* to prevent g from been inlined *)
      | n -> g (pred n)
    and g = function
      | 0 -> i
      | -1 -> f (-2)  (* to prevent f from been inlined *)
      | n -> f (pred n)
    in
    direct   := f i :: !direct;
    indirect := (fun () -> f i) :: !indirect
  done;
  let indirect = List.map (fun f -> f ()) !indirect in
  let direct = !direct in
  assert (indirect = direct)
#end