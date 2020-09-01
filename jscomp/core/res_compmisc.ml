
let init_path () =
  let dirs = !Clflags.include_dirs in
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs in
    Config.load_path :=
         List.rev_append exp_dirs (Clflags.std_include_dir ());
  Env.reset_cache ()

(* Return the initial environment in which compilation proceeds. *)

(* Note: do not do init_path() in initial_env, this breaks
   toplevel initialization (PR#1775) *)

let open_implicit_module m env =
  let lid = {Asttypes.loc = Location.in_file "command line";
             txt = Longident.parse m } in
  snd (Typemod.type_open_ Override env lid.loc lid)

let initial_env () =
  Ident.reinit();
  let initial =
    if Config.safe_string then Env.initial_safe_string
    else if !Clflags.unsafe_string then Env.initial_unsafe_string
    else Env.initial_safe_string
  in
  let env =
    if !Clflags.nopervasives then initial else
    open_implicit_module "Pervasives" initial
  in
  List.fold_left (fun env m ->
    open_implicit_module m env
  ) env (List.rev !Clflags.open_modules)

