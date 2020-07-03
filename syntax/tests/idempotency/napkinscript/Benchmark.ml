module Benchmark: sig
  type t

  val make: name:string -> ?time:Time.t -> f:(t -> unit) -> unit -> t
  val launch: t -> unit
  val report: t -> unit
end = struct
  type benchmarkResult = {
    n: int; (* number of iterations *)
    t: Time.t; (* total time taken *)
    bytes: float; (* bytes processed in one iteration *)
    memAllocs: float; (* total number of memory allocations in words*)
    memBytes: float; (* total number of bytes allocated *)
  }

  type t = {
    name: string;
    time: Time.t; (* how long should this benchmark run? *)
    mutable start: Time.t;
    mutable n: int; (* number of iterations *)
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
    print_endline (Format.sprintf "Benchmark ran during: %fms" (Time.print b.duration));
    print_endline (Format.sprintf "Avg time/op: %fms" ((Time.print b.duration) /. (float_of_int b.n)));
    print_endline (Format.sprintf "Allocs/op: %d" (int_of_float (b.netAllocs /.  (float_of_int b.n))));
    print_endline (Format.sprintf "B/op: %d" (int_of_float (b.netBytes /. (float_of_int b.n))));
    (* return (float64(r.Bytes) * float64(r.N) / 1e6) / r.T.Seconds() *)


    print_newline();
    ()

  let make ~name ?(time=Time.second) ~f () = {
    name;
    time;
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
    let stats = Gc.quick_stat() in
    stats.minor_words +. stats.major_words -. stats.promoted_words

  let startTimer b =
    if not b.timerOn then (
      let allocatedWords = mallocs() in
      b.startAllocs <- allocatedWords;
      b.startBytes <- allocatedWords *. 8.;
      b.start <- Time.now();
      b.timerOn <- true
    )

  let stopTimer b =
    if b.timerOn then (
      let allocatedWords = mallocs() in
      let diff = (Time.diff b.start (Time.now())) in
      b.duration <- Time.add b.duration diff;
      b.netAllocs <- b.netAllocs +. (allocatedWords -. b.startAllocs);
      b.netBytes <- b.netBytes +. (allocatedWords *. 8. -. b.startBytes);
      b.timerOn <- false
    )

  let resetTimer b =
    if b.timerOn then (
      let allocatedWords = mallocs() in
      b.startAllocs <- allocatedWords;
      b.netAllocs <- allocatedWords *. 8.;
    b.start <- Time.now();
    );
    b.netAllocs <- 0.;
    b.netBytes <- 0.

  let runIteration b n =
    Gc.full_major();
    b.n <- n;
    resetTimer b;
    startTimer b;
    b.benchFunc b;
    stopTimer b

  let launch b =
    let d = b.time in
    let n = ref 0 in
    while b.duration < d && !n < 1000000000 do
      n := !n + 1;
      runIteration b !n
    done
end
