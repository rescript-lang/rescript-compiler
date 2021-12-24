module Time: sig
  type t

  val now: unit -> t

  val toUint64: t -> int64
  (* let of_uint64_ns ns = ns *)

  val nanosecond: t
  val microsecond: t
  val millisecond: t
  val second: t
  val minute: t
  val hour: t

  val zero: t

  val diff: t -> t -> t
  val add: t -> t -> t
  val print: t -> float
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
  external init: unit -> unit = "caml_mach_initialize"
  let () = init()
  external now: unit -> t = "caml_mach_absolute_time"

  let diff t1 t2 = Int64.sub t2 t1
  let add t1 t2 = Int64.add t1 t2
  let print t =
    (Int64.to_float t) *. 1e-6
end

