(* command line flags *)
module Clflags: sig
  val recover: bool ref
  val profile: bool ref
  val bench: bool ref
  val print: string ref
  val width: int ref
  val origin: string ref
  val files: string list ref
  val interface: bool ref

  val parse: unit -> unit
  val outcome: bool ref
end = struct
  let recover = ref false
  let profile = ref false
  let bench = ref false
  let setRecover () = recover := true
  let width = ref 100

  let files = ref []
  let addFilename filename = files := filename::(!files)

  let print = ref ""
  let outcome = ref false
  let origin = ref ""
  let interface = ref false

  let usage = "Usage: napkinscript <options> <file>\nOptions are:"

  let spec = [
    ("-recover", Arg.Unit (fun () -> recover := true), "Emit partial ast");
    ("-bench", Arg.Unit (fun () -> bench := true), "Run internal benchmarks");
    ("-print", Arg.String (fun txt -> print := txt), "Print either binary, ocaml or ast");
    ("-parse", Arg.String (fun txt -> origin := txt), "Parse ocaml or napkinscript");
    ("-profile", Arg.Unit (fun () -> profile := true), "Enable performance profiling");
    ("-outcome", Arg.Bool (fun printOutcomeTree -> outcome := printOutcomeTree), "print outcometree");
    ("-width", Arg.Int (fun w -> width := w), "Specify the line length that the printer will wrap on" );
    ("-interface", Arg.Unit (fun () -> interface := true), "Parse as interface");
  ]

  let parse () = Arg.parse spec addFilename usage
end
