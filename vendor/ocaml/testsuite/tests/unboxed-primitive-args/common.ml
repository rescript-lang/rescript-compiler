open StdLabels

open Bigarray

type 'a typ =
  | Int       : int       typ
  | Int32     : int32     typ
  | Int64     : int64     typ
  | Nativeint : nativeint typ
  | Float     : float     typ

type 'a proto =
  | Ret : 'a typ -> 'a proto
  | Abs : 'a typ * 'b proto -> ('a -> 'b) proto

let ( ** ) x y = Abs (x, y)

(* This form is easier to process programmatically. We don't expose it as
   ocamlopt takes a really really long time to compile a constant list
   of these. *)
type simplified_test = Test : string * 'a * 'a proto -> simplified_test

type test =
  | T1 : string * ('a -> 'b) * 'a typ * 'b typ -> test
  | T2 : string * ('a -> 'b -> 'c) * 'a typ * 'b typ * 'c typ -> test
  | T3 : string * ('a -> 'b -> 'c -> 'd) *
         'a typ * 'b typ * 'c typ * 'd typ -> test
  | T4 : string * ('a -> 'b -> 'c -> 'd -> 'e) *
         'a typ * 'b typ * 'c typ * 'd typ * 'e typ -> test
  | T5 : string * ('a -> 'b -> 'c -> 'd -> 'e -> 'f) *
         'a typ * 'b typ * 'c typ * 'd typ * 'e typ * 'f typ -> test
  | T6 : string * ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) *
         'a typ * 'b typ * 'c typ * 'd typ * 'e typ * 'f typ * 'g typ -> test
  | T : string * 'a * 'a proto -> test

let expand_test = function
  | T1 (s, fn, a, b) -> Test (s, fn, a ** Ret b)
  | T2 (s, fn, a, b, c) -> Test (s, fn, a ** b ** Ret c)
  | T3 (s, fn, a, b, c, d) -> Test (s, fn, a ** b ** c ** Ret d)
  | T4 (s, fn, a, b, c, d, e) -> Test (s, fn, a ** b ** c ** d ** Ret e)
  | T5 (s, fn, a, b, c, d, e, f) ->
    Test (s, fn, a ** b ** c ** d ** e ** Ret f)
  | T6 (s, fn, a, b, c, d, e, f, g) ->
    Test (s, fn, a ** b ** c ** d ** e ** f ** Ret g)
  | T (s, fn, p) -> Test (s, fn, p)

let string_of : type a. a typ -> a -> string = function
  | Int       -> string_of_int
  | Int32     -> Printf.sprintf "%ldl"
  | Int64     -> Printf.sprintf "%LdL"
  | Nativeint -> Printf.sprintf "%ndn"
  | Float     ->
      fun f -> Printf.sprintf "float_of_bits 0x%LxL" (Int64.bits_of_float f)

let rec arity : type a. a proto -> int = function
  | Ret _ -> 0
  | Abs (_, p) -> 1 + arity p

module Buffer = struct
  type t = (char, int8_unsigned_elt, c_layout) Array1.t

  let arg_size = 8

  let create ~arity : t =
    Array1.create char c_layout ((arity + 1) * arg_size)

  let clear (t : t) = Array1.fill t '\000'

  let length : t -> int = Array1.dim

  external init_c_side : ocaml_buffer:t -> c_buffer:t -> unit
    = "test_set_buffers"

  external get_int32 : t -> int -> int32 = "%caml_bigstring_get32"
  external get_int64 : t -> int -> int64 = "%caml_bigstring_get64"
  external set_int32 : t -> int -> int32 -> unit = "%caml_bigstring_set32"
  external set_int64 : t -> int -> int64 -> unit = "%caml_bigstring_set64"

  let get_int32 t ~arg = get_int32 t (arg * arg_size)
  let get_int64 t ~arg = get_int64 t (arg * arg_size)
  let set_int32 t ~arg x = set_int32 t (arg * arg_size) x
  let set_int64 t ~arg x = set_int64 t (arg * arg_size) x

  let get_nativeint, set_nativeint =
    match Sys.word_size with
    | 32 -> ((fun t ~arg -> get_int32 t ~arg |> Nativeint.of_int32),
             (fun t ~arg x -> set_int32 t ~arg (Nativeint.to_int32 x)))
    | 64 -> ((fun t ~arg -> get_int64 t ~arg |> Int64.to_nativeint),
             (fun t ~arg x -> set_int64 t ~arg (Int64.of_nativeint x)))
    | n  -> Printf.ksprintf failwith "unknown word size (%d)" n

  let get_int =
    if Sys.word_size = 32 then
      fun buf ~arg -> get_int32 buf ~arg |> Int32.to_int
    else
      fun buf ~arg -> get_int64 buf ~arg |> Int64.to_int

  let set_int =
    if Sys.word_size = 32 then
      fun buf ~arg x -> set_int32 buf ~arg (Int32.of_int x)
    else
      fun buf ~arg x -> set_int64 buf ~arg (Int64.of_int x)

  let get_float buf ~arg = get_int64 buf ~arg |> Int64.float_of_bits
  let set_float buf ~arg x = set_int64 buf ~arg (Int64.bits_of_float x)

  let get : type a. a typ -> t -> arg:int -> a = function
    | Int       -> get_int
    | Int32     -> get_int32
    | Int64     -> get_int64
    | Nativeint -> get_nativeint
    | Float     -> get_float

  let set : type a. a typ -> t -> arg:int -> a -> unit = function
    | Int       -> set_int
    | Int32     -> set_int32
    | Int64     -> set_int64
    | Nativeint -> set_nativeint
    | Float     -> set_float

  (* This is almost a memcpy except that we use get/set which should
     ensure that the values in [dst] don't overflow. *)
  let copy_args ~src ~dst proto =
    let rec loop : type a. a proto -> int -> unit = fun proto arg ->
      match proto with
      | Ret typ ->
        set typ dst ~arg (get typ src ~arg)
      | Abs (typ, rest) ->
        set typ dst ~arg (get typ src ~arg);
        loop rest (arg + 1)
    in
    loop proto 0
end

let exec proto f ~ocaml_buffer ~c_buffer =
  let rec loop : type a. a proto -> a -> int -> unit = fun proto f arg ->
    match proto with
    | Ret typ ->
      Buffer.set typ c_buffer ~arg f
    | Abs (typ, rest) ->
      let x = Buffer.get typ ocaml_buffer ~arg in
      loop rest (f x) (arg + 1)
  in
  loop proto f 0

let strings_of_test_instance name proto buffer =
  let rec loop : type a. a proto -> int -> string list -> string list * string =
    fun proto arg acc ->
      match proto with
      | Ret typ ->
        (List.rev acc, string_of typ (Buffer.get typ buffer ~arg))
      | Abs (typ, rest) ->
        let s = string_of typ (Buffer.get typ buffer ~arg) in
        loop rest (arg + 1) (s :: acc)
  in
  loop proto 0 []

let typ_size : type a. a typ -> int = function
  | Int       -> Sys.word_size / 8
  | Int32     -> 4
  | Int64     -> 8
  | Nativeint -> Sys.word_size / 8
  | Float     -> 8

let rec sizes : type a. a proto -> int list = function
  | Ret typ         -> [typ_size typ]
  | Abs (typ, rest) -> typ_size typ :: sizes rest

let print_hex ~sizes ~arity buffer =
  let printf = Printf.printf in
  printf "(";
  for i = 0 to arity do
    if i = arity then
      printf ") -> "
    else if i > 0 then
      printf ", ";
    for ofs = i * Buffer.arg_size to i * Buffer.arg_size + sizes.(i) - 1 do
      printf "%02x" (Char.code buffer.{ofs});
    done;
  done

let printed_mismatches = ref 0

let print_mismatch name proto ~ocaml_buffer ~c_buffer =
  let printf = Printf.printf in
  printf "Mismatch for %s\n" name;
  let o_args, o_res = strings_of_test_instance name proto ocaml_buffer in
  let c_args, c_res = strings_of_test_instance name proto     c_buffer in
  let o_args, c_args =
    (* Align arguments *)
    List.map2 o_args c_args ~f:(fun a b ->
      let len_a = String.length a and len_b = String.length b in
      let len = max len_a len_b in
      (Printf.sprintf "%*s" len a,
       Printf.sprintf "%*s" len b))
    |> List.split
  in
  printf "ocaml side : (%s) -> %s\n" (String.concat ~sep:", " o_args) o_res;
  printf "c side     : (%s) -> %s\n" (String.concat ~sep:", " c_args) c_res;
  let sizes = sizes proto |> Array.of_list in
  let arity = arity proto in
  printf "ocaml side : "; print_hex ~sizes ~arity ocaml_buffer; printf "\n";
  printf "c side     : "; print_hex ~sizes ~arity     c_buffer; printf "\n";
  incr printed_mismatches;
  if !printed_mismatches >= 1000 then begin
    printf "Output truncated at 1000 failures.";
    exit 0
  end

external cleanup_normal
  :  int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> int -> int
  -> int = "" "test_cleanup_normal" [@@noalloc]

external cleanup_float
  :  float -> float -> float -> float -> float -> float -> float -> float
  -> float -> float -> float -> float -> float -> float -> float -> float
  -> float -> float -> float -> float -> float -> float -> float -> float
  -> float -> float -> float -> float -> float -> float -> float -> float
  -> float = "" "test_cleanup_float" [@@noalloc] [@@unboxed]

let cleanup_args_and_stack () =
  let _ : int =
    cleanup_normal
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  in
  let _ : float =
    cleanup_float
       0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
       0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
  in
  ()

let run_test ~random_data ~ocaml_buffer ~c_buffer (Test (name, f, proto)) =
  Buffer.clear ocaml_buffer;
  Buffer.clear c_buffer;
  Buffer.copy_args ~src:random_data ~dst:ocaml_buffer proto;
  cleanup_args_and_stack ();
  exec proto f ~ocaml_buffer ~c_buffer;
  let success = ocaml_buffer = c_buffer in
  if not success then print_mismatch name proto ~ocaml_buffer ~c_buffer;
  success

let run_tests tests =
  let tests = List.map tests ~f:expand_test in
  let max_args =
    List.fold_left tests ~init:0 ~f:(fun acc (Test (_, _, p)) ->
      max acc (arity p))
  in

  let ocaml_buffer = Buffer.create ~arity:max_args
  and     c_buffer = Buffer.create ~arity:max_args in
  Buffer.init_c_side ~ocaml_buffer ~c_buffer;

  let random_data = Buffer.create ~arity:max_args in
  let new_random_data () =
    for i = 0 to Buffer.length random_data - 1 do
      random_data.{i} <- char_of_int (Random.int 256)
    done
  in

  let failure = ref false in
  for i = 1 to 1000 do
    new_random_data ();
    List.iter tests ~f:(fun test ->
      if not (run_test ~random_data ~ocaml_buffer ~c_buffer test) then
        failure := true)
  done;
  exit (if !failure then 1 else 0)
