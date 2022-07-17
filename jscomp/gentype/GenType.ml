let version = Version.version

type cliCommand = Add of string | NoOp

let executeCliCommand ~printUsageAndExit cliCommand =
  match cliCommand with
  | Add s ->
      Log_.Color.forceColor := true;
      let splitColon = Str.split (Str.regexp ":") s in
      let cmt, mlast =
        match splitColon with
        | cmt :: rest ->
            let mlast = rest |> String.concat "" in
            (cmt, mlast)
        | _ -> assert false
      in
      let config = Paths.readConfig ~namespace:(cmt |> Paths.findNameSpace) in
      if !Debug.basic then Log_.item "Add %s  %s\n" cmt mlast;
      cmt |> GenTypeMain.processCmtFile ~config;
      exit 0
  | NoOp -> printUsageAndExit ()

let cli () =
  let cliCommand = ref NoOp in
  let usage = "genType version " ^ version in
  let versionAndExit () =
    print_endline usage;
    exit 0
  in
  let rec printUsageAndExit () =
    Arg.usage speclist usage;
    exit 0
  and setCliCommand command =
    if !cliCommand <> NoOp then printUsageAndExit ();
    cliCommand := command
  and setAdd s = Add s |> setCliCommand
  and speclist =
    [
      ("-cmt-add", Arg.String setAdd, "compile a .cmt[i] file");
      ("-version", Arg.Unit versionAndExit, "show version information and exit");
      ("--version", Arg.Unit versionAndExit, "show version information and exit");
    ]
  in
  let anonArg s = print_endline s
  in
  Arg.parse speclist anonArg usage;
  executeCliCommand ~printUsageAndExit !cliCommand
