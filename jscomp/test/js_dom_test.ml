
let suites = Mt.[
  (* These aren't available on node, so this is just to make sure it compiles *)

  ("Dom sanity check", (fun _ -> 
    ThrowAny(fun () -> Dom.Storage.(local |> get "key") |> ignore);
  ));

  ("Js.Dom sanity check", (fun _ -> 
    ThrowAny(fun () -> Js.Dom.Storage.(local |> get "key") |> ignore);
  ));
]

;; Mt.from_pair_suites __FILE__ suites
