module ResParser = Res_core
module Doc = Res_doc
module CommentTable = Res_comments_table
module Parser = Res_parser
module Printer = Res_printer

module IO : sig
  val readFile : string -> string
end = struct
  (* random chunk size: 2^15, TODO: why do we guess randomly? *)
  let chunkSize = 32768

  let readFile filename =
    let chan = open_in filename in
    let buffer = Buffer.create chunkSize in
    let chunk = (Bytes.create [@doesNotRaise]) chunkSize in
    let rec loop () =
      let len =
        try input chan chunk 0 chunkSize with Invalid_argument _ -> 0
      in
      if len == 0 then (
        close_in_noerr chan;
        Buffer.contents buffer)
      else (
        Buffer.add_subbytes buffer chunk 0 len;
        loop ())
    in
    loop ()
end

module Time : sig
  type t

  val now : unit -> t

  val toUint64 : t -> int64 [@@live]

  (* let of_uint64_ns ns = ns *)

  val nanosecond : t [@@live]
  val microsecond : t [@@live]
  val millisecond : t [@@live]
  val second : t [@@live]
  val minute : t [@@live]
  val hour : t [@@live]

  val zero : t

  val diff : t -> t -> t
  val add : t -> t -> t
  val print : t -> float
end = struct
  (* nanoseconds *)
  type t = int64

  let zero = 0L

  let toUint64 s = s

  let nanosecond = 1L
  let microsecond = Int64.mul 1000L nanosecond
  let millisecond = Int64.mul 1000L microsecond
  let second = Int64.mul 1000L millisecond
  let minute = Int64.mul 60L second
  let hour = Int64.mul 60L minute

  (* TODO: we could do this inside caml_absolute_time *)
  external init : unit -> unit = "caml_mach_initialize"
  let () = init ()
  external now : unit -> t = "caml_mach_absolute_time"

  let diff t1 t2 = Int64.sub t2 t1
  let add t1 t2 = Int64.add t1 t2
  let print t = Int64.to_float t *. 1e-6
end

module Benchmark : sig
  type t

  val make : name:string -> f:(t -> unit) -> unit -> t
  val launch : t -> unit
  val report : t -> unit
end = struct
  type t = {
    name: string;
    mutable start: Time.t;
    mutable n: int; (* current iterations count *)
    mutable duration: Time.t;
    benchFunc: t -> unit;
    mutable timerOn: bool;
    (* mutable result: benchmarkResult; *)
    (* The initial states *)
    mutable startAllocs: float;
    mutable startBytes: float;
    (* The net total of this test after being run. *)
    mutable netAllocs: float;
    mutable netBytes: float;
  }

  let report b =
    print_endline (Format.sprintf "Benchmark: %s" b.name);
    print_endline (Format.sprintf "Nbr of iterations: %d" b.n);
    print_endline
      (Format.sprintf "Benchmark ran during: %fms" (Time.print b.duration));
    print_endline
      (Format.sprintf "Avg time/op: %fms"
         (Time.print b.duration /. float_of_int b.n));
    print_endline
      (Format.sprintf "Allocs/op: %d"
         (int_of_float (b.netAllocs /. float_of_int b.n)));
    print_endline
      (Format.sprintf "B/op: %d"
         (int_of_float (b.netBytes /. float_of_int b.n)));

    (* return (float64(r.Bytes) * float64(r.N) / 1e6) / r.T.Seconds() *)
    print_newline ();
    ()

  let make ~name ~f () =
    {
      name;
      start = Time.zero;
      n = 0;
      benchFunc = f;
      duration = Time.zero;
      timerOn = false;
      startAllocs = 0.;
      startBytes = 0.;
      netAllocs = 0.;
      netBytes = 0.;
    }

  (* total amount of memory allocated by the program since it started in words *)
  let mallocs () =
    let stats = Gc.quick_stat () in
    stats.minor_words +. stats.major_words -. stats.promoted_words

  let startTimer b =
    if not b.timerOn then (
      let allocatedWords = mallocs () in
      b.startAllocs <- allocatedWords;
      b.startBytes <- allocatedWords *. 8.;
      b.start <- Time.now ();
      b.timerOn <- true)

  let stopTimer b =
    if b.timerOn then (
      let allocatedWords = mallocs () in
      let diff = Time.diff b.start (Time.now ()) in
      b.duration <- Time.add b.duration diff;
      b.netAllocs <- b.netAllocs +. (allocatedWords -. b.startAllocs);
      b.netBytes <- b.netBytes +. ((allocatedWords *. 8.) -. b.startBytes);
      b.timerOn <- false)

  let resetTimer b =
    if b.timerOn then (
      let allocatedWords = mallocs () in
      b.startAllocs <- allocatedWords;
      b.netAllocs <- allocatedWords *. 8.;
      b.start <- Time.now ());
    b.netAllocs <- 0.;
    b.netBytes <- 0.

  let runIteration b n =
    Gc.full_major ();
    b.n <- n;
    resetTimer b;
    startTimer b;
    b.benchFunc b;
    stopTimer b

  let launch b =
    (* 150 runs * all the benchmarks means around 1m of benchmark time *)
    for n = 1 to 150 do
      runIteration b n
    done
end

module Benchmarks : sig
  val run : unit -> unit
end = struct
  type action = Parse | Print
  let string_of_action action =
    match action with
    | Parse -> "parser"
    | Print -> "printer"

  (* TODO: we could at Reason here *)
  type lang = Ocaml | Rescript
  let string_of_lang lang =
    match lang with
    | Ocaml -> "ocaml"
    | Rescript -> "rescript"

  let parseOcaml src filename =
    let lexbuf = Lexing.from_string src in
    Location.init lexbuf filename;
    Parse.implementation lexbuf

  let parseRescript src filename =
    let p = Parser.make src filename in
    let structure = ResParser.parseImplementation p in
    assert (p.diagnostics == []);
    structure

  let benchmark filename lang action =
    let src = IO.readFile filename in
    let name =
      filename ^ " " ^ string_of_lang lang ^ " " ^ string_of_action action
    in
    let benchmarkFn =
      match (lang, action) with
      | Rescript, Parse ->
        fun _ ->
          let _ = Sys.opaque_identity (parseRescript src filename) in
          ()
      | Ocaml, Parse ->
        fun _ ->
          let _ = Sys.opaque_identity (parseOcaml src filename) in
          ()
      | Rescript, Print ->
        let p = Parser.make src filename in
        let ast = ResParser.parseImplementation p in
        fun _ ->
          let _ =
            Sys.opaque_identity
              (let cmtTbl = CommentTable.make () in
               let comments = List.rev p.Parser.comments in
               let () = CommentTable.walkStructure ast cmtTbl comments in
               Doc.toString ~width:80 (Printer.printStructure ast cmtTbl))
          in
          ()
      | _ -> fun _ -> ()
    in
    let b = Benchmark.make ~name ~f:benchmarkFn () in
    Benchmark.launch b;
    Benchmark.report b

  let run () =
    benchmark "./benchmarks/data/RedBlackTree.res" Rescript Parse;
    benchmark "./benchmarks/data/RedBlackTree.ml" Ocaml Parse;
    benchmark "./benchmarks/data/RedBlackTree.res" Rescript Print;
    benchmark "./benchmarks/data/RedBlackTreeNoComments.res" Rescript Print;
    benchmark "./benchmarks/data/Napkinscript.res" Rescript Parse;
    benchmark "./benchmarks/data/Napkinscript.ml" Ocaml Parse;
    benchmark "./benchmarks/data/Napkinscript.res" Rescript Print;
    benchmark "./benchmarks/data/HeroGraphic.res" Rescript Parse;
    benchmark "./benchmarks/data/HeroGraphic.ml" Ocaml Parse;
    benchmark "./benchmarks/data/HeroGraphic.res" Rescript Print
end

let () = Benchmarks.run ()
