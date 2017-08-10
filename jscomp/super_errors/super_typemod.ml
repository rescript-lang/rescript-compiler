(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Typemod.Error (loc, env, err) ->
         Some (Location.error_of_printer loc (fun ppf ->
          Super_misc.setup_colors ppf;
          Typemod.report_error env ppf
        ) err)
      | Typemod.Error_forward err ->
        Some err
      | _ ->
        None
    )
