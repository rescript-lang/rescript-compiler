module Bsb_dir_index :
  sig
    type t = private int[@@ocaml.doc
                          " Used to index [.bsbuildcache] may not be needed if we flatten dev \n  into  a single group\n"]
    val is_lib_dir : t -> bool
    val of_int : int -> t
  end =
  struct
    type t = int
    external of_int : int -> t = "%identity"[@@ocaml.doc
                                              " \n   0 : lib \n   1 : dev 1 \n   2 : dev 2 \n"]
    let lib_dir_index = 0
    let is_lib_dir x = x = lib_dir_index
  end 
module Ext_bytes :
  sig
    external unsafe_blit_string :
      string -> int -> bytes -> int -> int -> unit = "caml_blit_string"
    [@@noalloc ]
  end =
  struct
    external unsafe_blit_string :
      string -> int -> bytes -> int -> int -> unit = "caml_blit_string"
    [@@noalloc ]
  end 
module Ext_buffer :
  sig
    [@@@ocaml.text
      " Extensible buffers.\n\n   This module implements buffers that automatically expand\n   as necessary.  It provides accumulative concatenation of strings\n   in quasi-linear time (instead of quadratic time when strings are\n   concatenated pairwise).\n"]
    type t[@@ocaml.doc " The abstract type of buffers. "]
    val create : int -> t[@@ocaml.doc
                           " [create n] returns a fresh buffer, initially empty.\n   The [n] parameter is the initial size of the internal byte sequence\n   that holds the buffer contents. That byte sequence is automatically\n   reallocated when more than [n] characters are stored in the buffer,\n   but shrinks back to [n] characters when [reset] is called.\n   For best performance, [n] should be of the same order of magnitude\n   as the number of characters that are expected to be stored in\n   the buffer (for instance, 80 for a buffer that holds one output\n   line).  Nothing bad will happen if the buffer grows beyond that\n   limit, however. In doubt, take [n = 16] for instance.\n   If [n] is not between 1 and {!Sys.max_string_length}, it will\n   be clipped to that interval. "]
    val contents : t -> string[@@ocaml.doc
                                " Return a copy of the current contents of the buffer.\n    The buffer itself is unchanged. "]
    val length : t -> int[@@ocaml.doc
                           " Return the number of characters currently contained in the buffer. "]
    [@@@ocaml.text " Empty the buffer. "]
    val add_char : t -> char -> unit[@@ocaml.doc
                                      " [add_char b c] appends the character [c] at the end of the buffer [b]. "]
    val add_string : t -> string -> unit[@@ocaml.doc
                                          " [add_string b s] appends the string [s] at the end of the buffer [b]. "]
    [@@@ocaml.text
      " [add_string b s] appends the string [s] at the end of the buffer [b].\n    @since 4.02 "]
    [@@@ocaml.text
      " [add_substring b s ofs len] takes [len] characters from offset\n   [ofs] in string [s] and appends them at the end of the buffer [b]. "]
    [@@@ocaml.text
      " [add_substring b s ofs len] takes [len] characters from offset\n    [ofs] in byte sequence [s] and appends them at the end of the buffer [b].\n    @since 4.02 "]
    [@@@ocaml.text
      " [add_buffer b1 b2] appends the current contents of buffer [b2]\n   at the end of buffer [b1].  [b2] is not modified. "]
    [@@@ocaml.text
      " [add_channel b ic n] reads exactly [n] character from the\n   input channel [ic] and stores them at the end of buffer [b].\n   Raise [End_of_file] if the channel contains fewer than [n]\n   characters. "]
    val output_buffer : out_channel -> t -> unit[@@ocaml.doc
                                                  " [output_buffer oc b] writes the current contents of buffer [b]\n   on the output channel [oc]. "]
    val not_equal : t -> string -> bool
    val add_string_char : t -> string -> char -> unit
    val add_char_string : t -> char -> string -> unit
  end =
  struct
    type t =
      {
      mutable buffer: bytes ;
      mutable position: int ;
      mutable length: int ;
      initial_buffer: bytes }
    let create n =
      let n = if n < 1 then 1 else n in
      let n = if n > Sys.max_string_length then Sys.max_string_length else n in
      let s = Bytes.create n in
      { buffer = s; position = 0; length = n; initial_buffer = s }
    let contents b = Bytes.sub_string b.buffer 0 b.position
    let length b = b.position
    let resize b more =
      let len = b.length in
      let new_len = ref len in
      while (b.position + more) > (!new_len) do new_len := (2 * (!new_len))
        done;
      if (!new_len) > Sys.max_string_length
      then
        (if (b.position + more) <= Sys.max_string_length
         then new_len := Sys.max_string_length
         else failwith "Ext_buffer.add: cannot grow buffer");
      (let new_buffer = Bytes.create (!new_len) in
       Bytes.blit b.buffer 0 new_buffer 0 b.position;
       b.buffer <- new_buffer;
       b.length <- (!new_len);
       assert ((b.position + more) <= b.length))
    let add_char b c =
      let pos = b.position in
      if pos >= b.length then resize b 1;
      Bytes.unsafe_set b.buffer pos c;
      b.position <- (pos + 1)
    let add_string b s =
      let len = String.length s in
      let new_position = b.position + len in
      if new_position > b.length then resize b len;
      Ext_bytes.unsafe_blit_string s 0 b.buffer b.position len;
      b.position <- new_position
    let add_string_char b s c =
      let s_len = String.length s in
      let len = s_len + 1 in
      let new_position = b.position + len in
      if new_position > b.length then resize b len;
      (let b_buffer = b.buffer in
       Ext_bytes.unsafe_blit_string s 0 b_buffer b.position s_len;
       Bytes.unsafe_set b_buffer (new_position - 1) c;
       b.position <- new_position)
    let add_char_string b c s =
      let s_len = String.length s in
      let len = s_len + 1 in
      let new_position = b.position + len in
      if new_position > b.length then resize b len;
      (let b_buffer = b.buffer in
       let b_position = b.position in
       Bytes.unsafe_set b_buffer b_position c;
       Ext_bytes.unsafe_blit_string s 0 b_buffer (b_position + 1) s_len;
       b.position <- new_position)
    let output_buffer oc b = output oc b.buffer 0 b.position
    external unsafe_string :
      bytes -> int -> int -> Digest.t = "caml_md5_string"
    let rec not_equal_aux (b : bytes) (s : string) i len =
      if i >= len
      then false
      else
        ((Bytes.unsafe_get b i) <> (String.unsafe_get s i)) ||
          (not_equal_aux b s (i + 1) len)
    let not_equal (b : t) (s : string) =
      let b_len = b.position in
      let s_len = String.length s in
      (b_len <> s_len) || (not_equal_aux b.buffer s 0 s_len)[@@ocaml.doc
                                                              " avoid a large copy "]
  end 
module Ext_list :
  sig
    [@@@ocaml.text
      "\n\n   {[length xs = length ys + n ]}\n   input n should be positive \n   TODO: input checking\n"]
    [@@@ocaml.text
      " [find_opt f l] returns [None] if all return [None],  \n    otherwise returns the first one. \n"]
    val iter : 'a list -> ('a -> unit) -> unit
  end =
  struct
    let rec map l f =
      match l with
      | [] -> []
      | x1::[] -> let y1 = f x1 in [y1]
      | x1::x2::[] -> let y1 = f x1 in let y2 = f x2 in [y1; y2]
      | x1::x2::x3::[] ->
          let y1 = f x1 in let y2 = f x2 in let y3 = f x3 in [y1; y2; y3]
      | x1::x2::x3::x4::[] ->
          let y1 = f x1 in
          let y2 = f x2 in let y3 = f x3 in let y4 = f x4 in [y1; y2; y3; y4]
      | x1::x2::x3::x4::x5::tail ->
          let y1 = f x1 in
          let y2 = f x2 in
          let y3 = f x3 in
          let y4 = f x4 in
          let y5 = f x5 in y1 :: y2 :: y3 :: y4 :: y5 :: (map tail f)
    let rec has_string l f =
      match l with
      | [] -> false
      | x1::[] -> x1 = f
      | x1::x2::[] -> (x1 = f) || (x2 = f)
      | x1::x2::x3::[] -> (x1 = f) || ((x2 = f) || (x3 = f))
      | x1::x2::x3::x4 ->
          (x1 = f) || ((x2 = f) || ((x3 = f) || (has_string x4 f)))
    let rec map_combine l1 l2 f =
      match (l1, l2) with
      | ([], []) -> []
      | (a1::l1, a2::l2) -> ((f a1), a2) :: (map_combine l1 l2 f)
      | (_, _) -> invalid_arg "Ext_list.map_combine"
    let rec map_split_opt (xs : 'a list) (f : 'a -> ('b option * 'c option))
      =
      (match xs with
       | [] -> ([], [])
       | x::xs ->
           let (c, d) = f x in
           let (cs, ds) = map_split_opt xs f in
           (((match c with | Some c -> c :: cs | None -> cs)),
             ((match d with | Some d -> d :: ds | None -> ds))) : ('b list *
                                                                    'c list))
    let rec map_snd l f =
      match l with
      | [] -> []
      | (v1, x1)::[] -> let y1 = f x1 in [(v1, y1)]
      | (v1, x1)::(v2, x2)::[] ->
          let y1 = f x1 in let y2 = f x2 in [(v1, y1); (v2, y2)]
      | (v1, x1)::(v2, x2)::(v3, x3)::[] ->
          let y1 = f x1 in
          let y2 = f x2 in let y3 = f x3 in [(v1, y1); (v2, y2); (v3, y3)]
      | (v1, x1)::(v2, x2)::(v3, x3)::(v4, x4)::[] ->
          let y1 = f x1 in
          let y2 = f x2 in
          let y3 = f x3 in
          let y4 = f x4 in [(v1, y1); (v2, y2); (v3, y3); (v4, y4)]
      | (v1, x1)::(v2, x2)::(v3, x3)::(v4, x4)::(v5, x5)::tail ->
          let y1 = f x1 in
          let y2 = f x2 in
          let y3 = f x3 in
          let y4 = f x4 in
          let y5 = f x5 in (v1, y1) :: (v2, y2) :: (v3, y3) :: (v4, y4) ::
            (v5, y5) :: (map_snd tail f)
    let rec map_last l f =
      match l with
      | [] -> []
      | x1::[] -> let y1 = f true x1 in [y1]
      | x1::x2::[] -> let y1 = f false x1 in let y2 = f true x2 in [y1; y2]
      | x1::x2::x3::[] ->
          let y1 = f false x1 in
          let y2 = f false x2 in let y3 = f true x3 in [y1; y2; y3]
      | x1::x2::x3::x4::[] ->
          let y1 = f false x1 in
          let y2 = f false x2 in
          let y3 = f false x3 in let y4 = f true x4 in [y1; y2; y3; y4]
      | x1::x2::x3::x4::tail ->
          let y1 = f false x1 in
          let y2 = f false x2 in
          let y3 = f false x3 in
          let y4 = f false x4 in y1 :: y2 :: y3 :: y4 :: (map_last tail f)
    let rec mapi_aux lst i f =
      match lst with
      | [] -> []
      | a::l -> let r = f i a in r :: (mapi_aux l (i + 1) f)
    let rec last xs =
      match xs with
      | x::[] -> x
      | _::tl -> last tl
      | [] -> invalid_arg "Ext_list.last"
    let rec append_aux l1 l2 =
      match l1 with
      | [] -> l2
      | a0::[] -> a0 :: l2
      | a0::a1::[] -> a0 :: a1 :: l2
      | a0::a1::a2::[] -> a0 :: a1 :: a2 :: l2
      | a0::a1::a2::a3::[] -> a0 :: a1 :: a2 :: a3 :: l2
      | a0::a1::a2::a3::a4::[] -> a0 :: a1 :: a2 :: a3 :: a4 :: l2
      | a0::a1::a2::a3::a4::rest -> a0 :: a1 :: a2 :: a3 :: a4 ::
          (append_aux rest l2)
    let append l1 l2 = match l2 with | [] -> l1 | _ -> append_aux l1 l2
    let rec map_append l1 l2 f =
      match l1 with
      | [] -> l2
      | a0::[] -> (f a0) :: l2
      | a0::a1::[] -> let b0 = f a0 in let b1 = f a1 in b0 :: b1 :: l2
      | a0::a1::a2::[] ->
          let b0 = f a0 in
          let b1 = f a1 in let b2 = f a2 in b0 :: b1 :: b2 :: l2
      | a0::a1::a2::a3::[] ->
          let b0 = f a0 in
          let b1 = f a1 in
          let b2 = f a2 in let b3 = f a3 in b0 :: b1 :: b2 :: b3 :: l2
      | a0::a1::a2::a3::a4::[] ->
          let b0 = f a0 in
          let b1 = f a1 in
          let b2 = f a2 in
          let b3 = f a3 in let b4 = f a4 in b0 :: b1 :: b2 :: b3 :: b4 :: l2
      | a0::a1::a2::a3::a4::rest ->
          let b0 = f a0 in
          let b1 = f a1 in
          let b2 = f a2 in
          let b3 = f a3 in
          let b4 = f a4 in b0 :: b1 :: b2 :: b3 :: b4 ::
            (map_append rest l2 f)
    let rec fold_right l acc f =
      match l with
      | [] -> acc
      | a0::[] -> f a0 acc
      | a0::a1::[] -> f a0 (f a1 acc)
      | a0::a1::a2::[] -> f a0 (f a1 (f a2 acc))
      | a0::a1::a2::a3::[] -> f a0 (f a1 (f a2 (f a3 acc)))
      | a0::a1::a2::a3::a4::[] -> f a0 (f a1 (f a2 (f a3 (f a4 acc))))
      | a0::a1::a2::a3::a4::rest ->
          f a0 (f a1 (f a2 (f a3 (f a4 (fold_right rest acc f)))))
    let rec fold_right2 l r acc f =
      match (l, r) with
      | ([], []) -> acc
      | (a0::[], b0::[]) -> f a0 b0 acc
      | (a0::a1::[], b0::b1::[]) -> f a0 b0 (f a1 b1 acc)
      | (a0::a1::a2::[], b0::b1::b2::[]) -> f a0 b0 (f a1 b1 (f a2 b2 acc))
      | (a0::a1::a2::a3::[], b0::b1::b2::b3::[]) ->
          f a0 b0 (f a1 b1 (f a2 b2 (f a3 b3 acc)))
      | (a0::a1::a2::a3::a4::[], b0::b1::b2::b3::b4::[]) ->
          f a0 b0 (f a1 b1 (f a2 b2 (f a3 b3 (f a4 b4 acc))))
      | (a0::a1::a2::a3::a4::arest, b0::b1::b2::b3::b4::brest) ->
          f a0 b0
            (f a1 b1
               (f a2 b2 (f a3 b3 (f a4 b4 (fold_right2 arest brest acc f)))))
      | (_, _) -> invalid_arg "Ext_list.fold_right2"
    let rec map2 l r f =
      match (l, r) with
      | ([], []) -> []
      | (a0::[], b0::[]) -> [f a0 b0]
      | (a0::a1::[], b0::b1::[]) ->
          let c0 = f a0 b0 in let c1 = f a1 b1 in [c0; c1]
      | (a0::a1::a2::[], b0::b1::b2::[]) ->
          let c0 = f a0 b0 in
          let c1 = f a1 b1 in let c2 = f a2 b2 in [c0; c1; c2]
      | (a0::a1::a2::a3::[], b0::b1::b2::b3::[]) ->
          let c0 = f a0 b0 in
          let c1 = f a1 b1 in
          let c2 = f a2 b2 in let c3 = f a3 b3 in [c0; c1; c2; c3]
      | (a0::a1::a2::a3::a4::[], b0::b1::b2::b3::b4::[]) ->
          let c0 = f a0 b0 in
          let c1 = f a1 b1 in
          let c2 = f a2 b2 in
          let c3 = f a3 b3 in let c4 = f a4 b4 in [c0; c1; c2; c3; c4]
      | (a0::a1::a2::a3::a4::arest, b0::b1::b2::b3::b4::brest) ->
          let c0 = f a0 b0 in
          let c1 = f a1 b1 in
          let c2 = f a2 b2 in
          let c3 = f a3 b3 in
          let c4 = f a4 b4 in c0 :: c1 :: c2 :: c3 :: c4 ::
            (map2 arest brest f)
      | (_, _) -> invalid_arg "Ext_list.map2"
    let rec fold_left_with_offset l accu i f =
      match l with
      | [] -> accu
      | a::l -> fold_left_with_offset l (f a accu i) (i + 1) f
    let rec filter_map xs (f : 'a -> 'b option) =
      match xs with
      | [] -> []
      | y::ys ->
          (match f y with
           | None -> filter_map ys f
           | Some z -> z :: (filter_map ys f))
    let rec exclude (xs : 'a list) (p : 'a -> bool) =
      (match xs with
       | [] -> []
       | x::xs -> if p x then exclude xs p else x :: (exclude xs p) : 
      'a list)
    let rec exclude_with_val l p =
      match l with
      | [] -> None
      | a0::xs ->
          if p a0
          then Some (exclude xs p)
          else
            (match xs with
             | [] -> None
             | a1::rest ->
                 if p a1
                 then Some (a0 :: (exclude rest p))
                 else
                   (match exclude_with_val rest p with
                    | None -> None
                    | Some rest -> Some (a0 :: a1 :: rest)))
    let rec same_length xs ys =
      match (xs, ys) with
      | ([], []) -> true
      | (_::xs, _::ys) -> same_length xs ys
      | (_, _) -> false
    let rec rev_append l1 l2 =
      match l1 with
      | [] -> l2
      | a0::[] -> a0 :: l2
      | a0::a1::[] -> a1 :: a0 :: l2
      | a0::a1::a2::rest -> rev_append rest (a2 :: a1 :: a0 :: l2)
    let rev l = rev_append l []
    let rec small_split_at n acc l =
      if n <= 0
      then ((rev acc), l)
      else
        (match l with
         | x::xs -> small_split_at (n - 1) (x :: acc) xs
         | _ -> invalid_arg "Ext_list.split_at")
    let rec split_at_last_aux acc x =
      match x with
      | [] -> invalid_arg "Ext_list.split_at_last"
      | x::[] -> ((rev acc), x)
      | y0::ys -> split_at_last_aux (y0 :: acc) ys
    let rec filter_map2 xs ys (f : 'a -> 'b -> 'c option) =
      match (xs, ys) with
      | ([], []) -> []
      | (u::us, v::vs) ->
          (match f u v with
           | None -> filter_map2 us vs f
           | Some z -> z :: (filter_map2 us vs f))
      | _ -> invalid_arg "Ext_list.filter_map2"
    let rec rev_map_append l1 l2 f =
      match l1 with | [] -> l2 | a::l -> rev_map_append l ((f a) :: l2) f
    let rec flat_map_aux f acc append lx =
      match lx with
      | [] -> rev_append acc append
      | a0::rest ->
          let new_acc =
            match f a0 with
            | [] -> acc
            | a0::[] -> a0 :: acc
            | a0::a1::[] -> a1 :: a0 :: acc
            | a0::a1::a2::rest -> rev_append rest (a2 :: a1 :: a0 :: acc) in
          flat_map_aux f new_acc append rest[@@ocaml.doc
                                              " It is not worth loop unrolling, \n    it is already tail-call, and we need to be careful \n    about evaluation order when unroll\n"]
    let rec length_compare l n =
      if n < 0
      then `Gt
      else
        (match l with
         | _::xs -> length_compare xs (n - 1)
         | [] -> if n = 0 then `Eq else `Lt)
    let rec length_ge l n =
      if n > 0
      then match l with | _::tl -> length_ge tl (n - 1) | [] -> false
      else true[@@ocaml.doc "\n\n   {[length xs = length ys + n ]}\n"]
    let rec length_larger_than_n xs ys n =
      match (xs, ys) with
      | (_, []) -> (length_compare xs n) = `Eq
      | (_::xs, _::ys) -> length_larger_than_n xs ys n
      | ([], _) -> false[@@ocaml.doc
                          "\n\n   {[length xs = length ys + n ]}\n"]
    let rec group (eq : 'a -> 'a -> bool) lst =
      match lst with | [] -> [] | x::xs -> aux eq x (group eq xs)
    and aux eq (x : 'a) (xss : 'a list list) =
      (match xss with
       | [] -> [[x]]
       | (y0::_ as y)::ys ->
           if eq x y0 then (x :: y) :: ys else y :: (aux eq x ys)
       | _::_ -> assert false : 'a list list)
    let rec drop h n =
      if n < 0
      then invalid_arg "Ext_list.drop"
      else
        if n = 0
        then h
        else
          (match h with
           | [] -> invalid_arg "Ext_list.drop"
           | _::tl -> drop tl (n - 1))
    let rec find_first x p =
      match x with
      | [] -> None
      | x::l -> if p x then Some x else find_first l p
    let rec find_first_not xs p =
      match xs with
      | [] -> None
      | a::l -> if p a then find_first_not l p else Some a
    let rec rev_iter l f =
      match l with
      | [] -> ()
      | x1::[] -> f x1
      | x1::x2::[] -> (f x2; f x1)
      | x1::x2::x3::[] -> (f x3; f x2; f x1)
      | x1::x2::x3::x4::[] -> (f x4; f x3; f x2; f x1)
      | x1::x2::x3::x4::x5::tail ->
          (rev_iter tail f; f x5; f x4; f x3; f x2; f x1)
    let rec iter l f =
      match l with
      | [] -> ()
      | x1::[] -> f x1
      | x1::x2::[] -> (f x1; f x2)
      | x1::x2::x3::[] -> (f x1; f x2; f x3)
      | x1::x2::x3::x4::[] -> (f x1; f x2; f x3; f x4)
      | x1::x2::x3::x4::x5::tail ->
          (f x1; f x2; f x3; f x4; f x5; iter tail f)
    let rec for_all lst p =
      match lst with | [] -> true | a::l -> (p a) && (for_all l p)
    let rec for_all_snd lst p =
      match lst with | [] -> true | (_, a)::l -> (p a) && (for_all_snd l p)
    let rec for_all2_no_exn l1 l2 p =
      match (l1, l2) with
      | ([], []) -> true
      | (a1::l1, a2::l2) -> (p a1 a2) && (for_all2_no_exn l1 l2 p)
      | (_, _) -> false
    let rec find_opt xs p =
      match xs with
      | [] -> None
      | x::l -> (match p x with | Some _ as v -> v | None -> find_opt l p)
    let rec find_def xs p def =
      match xs with
      | [] -> def
      | x::l -> (match p x with | Some v -> v | None -> find_def l p def)
    let rec split_map l f =
      match l with
      | [] -> ([], [])
      | x1::[] -> let (a0, b0) = f x1 in ([a0], [b0])
      | x1::x2::[] ->
          let (a1, b1) = f x1 in let (a2, b2) = f x2 in ([a1; a2], [b1; b2])
      | x1::x2::x3::[] ->
          let (a1, b1) = f x1 in
          let (a2, b2) = f x2 in
          let (a3, b3) = f x3 in ([a1; a2; a3], [b1; b2; b3])
      | x1::x2::x3::x4::[] ->
          let (a1, b1) = f x1 in
          let (a2, b2) = f x2 in
          let (a3, b3) = f x3 in
          let (a4, b4) = f x4 in ([a1; a2; a3; a4], [b1; b2; b3; b4])
      | x1::x2::x3::x4::x5::tail ->
          let (a1, b1) = f x1 in
          let (a2, b2) = f x2 in
          let (a3, b3) = f x3 in
          let (a4, b4) = f x4 in
          let (a5, b5) = f x5 in
          let (ass, bss) = split_map tail f in
          ((a1 :: a2 :: a3 :: a4 :: a5 :: ass), (b1 :: b2 :: b3 :: b4 :: b5
            :: bss))
    let rec assoc_by_string lst (k : string) def =
      match lst with
      | [] -> (match def with | None -> assert false | Some x -> x)
      | (k1, v1)::rest -> if k1 = k then v1 else assoc_by_string rest k def
    let rec assoc_by_int lst (k : int) def =
      match lst with
      | [] -> (match def with | None -> assert false | Some x -> x)
      | (k1, v1)::rest -> if k1 = k then v1 else assoc_by_int rest k def
    let rec nth_aux l n =
      match l with
      | [] -> None
      | a::l -> if n = 0 then Some a else nth_aux l (n - 1)
    let rec iter_snd lst f =
      match lst with | [] -> () | (_, x)::xs -> (f x; iter_snd xs f)
    let rec iter_fst lst f =
      match lst with | [] -> () | (x, _)::xs -> (f x; iter_fst xs f)
    let rec exists l p =
      match l with | [] -> false | x::xs -> (p x) || (exists xs p)
    let rec exists_fst l p =
      match l with | [] -> false | (a, _)::l -> (p a) || (exists_fst l p)
    let rec exists_snd l p =
      match l with | [] -> false | (_, a)::l -> (p a) || (exists_snd l p)
    let rec concat_append (xss : 'a list list) (xs : 'a list) =
      (match xss with | [] -> xs | l::r -> append l (concat_append r xs) : 
      'a list)
    let rec fold_left l accu f =
      match l with | [] -> accu | a::l -> fold_left l (f accu a) f
    let rec fold_left2 l1 l2 accu f =
      match (l1, l2) with
      | ([], []) -> accu
      | (a1::l1, a2::l2) -> fold_left2 l1 l2 (f a1 a2 accu) f
      | (_, _) -> invalid_arg "Ext_list.fold_left2"
    let rec mem_string (xs : string list) (x : string) =
      match xs with | [] -> false | a::l -> (a = x) || (mem_string l x)
  end 
module Bsb_helper_arg :
  sig
    type spec =
      | Unit of (unit -> unit) [@dead "Bsb_helper_arg.spec.Unit"]
      | Set of bool ref [@dead "Bsb_helper_arg.spec.Set"]
      | String of (string -> unit) 
      | Set_string of string ref 
      | Int of (int -> unit) [@dead "Bsb_helper_arg.spec.Int"]
      | Set_int of int ref 
    type key = string
    type doc = string
    type usage_msg = string
    type anon_fun = string -> unit
    val parse_exn : (key * spec * doc) list -> anon_fun -> usage_msg -> unit
  end =
  struct
    type key = string
    type doc = string
    type usage_msg = string
    type anon_fun = string -> unit
    type spec =
      | Unit of (unit -> unit) 
      | Set of bool ref 
      | String of (string -> unit) 
      | Set_string of string ref 
      | Int of (int -> unit) 
      | Set_int of int ref 
    exception Bad of string 
    type error =
      | Unknown of string 
      | Wrong of string * string * string 
      | Missing of string 
      | Message of string 
    exception Stop of error 
    type t = (string * spec * string) list
    let rec assoc3 (x : string) (l : t) =
      match l with
      | [] -> None
      | (y1, y2, _y3)::_t when y1 = x -> Some y2
      | _::t -> assoc3 x t
    let usage_b (buf : Ext_buffer.t) speclist errmsg =
      let print_spec buf (key, _spec, doc) =
        if doc <> ""
        then
          (Ext_buffer.add_string buf "  ";
           Ext_buffer.add_string_char buf key ' ';
           Ext_buffer.add_string_char buf doc '\n') in
      Ext_buffer.add_string_char buf errmsg '\n';
      Ext_list.iter speclist (print_spec buf)
    let stop_raise progname (error : error) speclist errmsg =
      let b = Ext_buffer.create 200 in
      (match error with
       | Unknown ("-help"|"--help"|"-h") ->
           (usage_b b speclist errmsg;
            output_string stdout (Ext_buffer.contents b);
            exit 0)
       | Unknown s ->
           (Ext_buffer.add_string_char b progname ':';
            Ext_buffer.add_string b " unknown option '";
            Ext_buffer.add_string b s;
            Ext_buffer.add_string b "'.\n")
       | Missing s ->
           (Ext_buffer.add_string_char b progname ':';
            Ext_buffer.add_string b " option '";
            Ext_buffer.add_string b s;
            Ext_buffer.add_string b "' needs an argument.\n")
       | Wrong (opt, arg, expected) ->
           (Ext_buffer.add_string_char b progname ':';
            Ext_buffer.add_string b " wrong argument '";
            Ext_buffer.add_string b arg;
            Ext_buffer.add_string b "'; option '";
            Ext_buffer.add_string b opt;
            Ext_buffer.add_string b "' expects ";
            Ext_buffer.add_string b expected;
            Ext_buffer.add_string b ".\n")
       | Message s ->
           (Ext_buffer.add_string_char b progname ':';
            Ext_buffer.add_char_string b ' ' s;
            Ext_buffer.add_string b ".\n"));
      usage_b b speclist errmsg;
      raise (Bad (Ext_buffer.contents b))
    let parse_exn (speclist : t) anonfun errmsg =
      let argv = Sys.argv in
      let stop_raise error = stop_raise (argv.(0)) error speclist errmsg in
      let l = Array.length argv in
      let current = ref 1 in
      while (!current) < l do
        let s = argv.(!current) in
        if (s <> "") && ((s.[0]) = '-')
        then
          let action =
            match assoc3 s speclist with
            | Some action -> action
            | None -> stop_raise (Unknown s) in
          ((try
              let treat_action =
                function
                | Unit f -> f ()
                | Set r -> r := true
                | String f when ((!current) + 1) < l ->
                    (f (argv.((!current) + 1)); incr current)
                | Set_string r when ((!current) + 1) < l ->
                    (r := (argv.((!current) + 1)); incr current)
                | Int f when ((!current) + 1) < l ->
                    let arg = argv.((!current) + 1) in
                    ((match int_of_string arg with
                      | i -> f i
                      | exception _ ->
                          raise (Stop (Wrong (s, arg, "an integer"))));
                     incr current)
                | Set_int r when ((!current) + 1) < l ->
                    let arg = argv.((!current) + 1) in
                    (r :=
                       ((try int_of_string arg
                         with
                         | _ -> raise (Stop (Wrong (s, arg, "an integer")))));
                     incr current)
                | _ -> raise (Stop (Missing s)) in
              treat_action action
            with | Bad m -> stop_raise (Message m) | Stop e -> stop_raise e);
           incr current)
        else
          ((try anonfun s with | Bad m -> stop_raise (Message m));
           incr current)
        done
  end 
module Ext_digest : sig val length : int end = struct let length = 16 end 
module Ext_pervasives :
  sig
    [@@@ocaml.text
      " Extension to standard library [Pervavives] module, safe to open \n  "]
    external reraise : exn -> 'a = "%reraise"
    val finally : 'a -> clean:('a -> 'c) -> ('a -> 'b) -> 'b
    [@@@ocaml.text
      " Copied from {!Btype.hash_variant}:\n    need sync up and add test case\n "]
    val parse_nat_of_string : string -> int ref -> int
  end =
  struct
    external reraise : exn -> 'a = "%reraise"
    let finally v ~clean:action  f =
      match f v with
      | exception e -> (action v; reraise e)
      | e -> (action v; e)
    let rec int_of_string_aux s acc off len =
      if off >= len
      then acc
      else
        (let d = (Char.code (String.unsafe_get s off)) - 48 in
         if (d >= 0) && (d <= 9)
         then int_of_string_aux s ((10 * acc) + d) (off + 1) len
         else (-1))
    let parse_nat_of_string (s : string) (cursor : int ref) =
      let current = !cursor in
      assert (current >= 0);
      (let acc = ref 0 in
       let s_len = String.length s in
       let todo = ref true in
       let cur = ref current in
       while (!todo) && ((!cursor) < s_len) do
         (let d = (Char.code (String.unsafe_get s (!cur))) - 48 in
          if (d >= 0) && (d <= 9)
          then (acc := ((10 * (!acc)) + d); incr cur)
          else todo := false)
         done;
       cursor := (!cur);
       !acc)[@@ocaml.doc " return index "]
  end 
module Ext_io : sig val load_file : string -> string end =
  struct
    let load_file f =
      Ext_pervasives.finally (open_in_bin f) ~clean:close_in
        (fun ic ->
           let n = in_channel_length ic in
           let s = Bytes.create n in
           really_input ic s 0 n; Bytes.unsafe_to_string s)[@@ocaml.doc
                                                             " on 32 bit , there are 16M limitation "]
  end 
module Ext_string :
  sig
    [@@@ocaml.text
      " Extension to the standard library [String] module, fixed some bugs like\n    avoiding locale sensitivity "]
    [@@@ocaml.text
      "\n  [extract_until s cursor sep]\n   When [sep] not found, the cursor is updated to -1,\n   otherwise cursor is increased to 1 + [sep_position]\n   User can not determine whether it is found or not by\n   telling the return string is empty since \n   \"\\n\\n\" would result in an empty string too.\n"]
    val index_count : string -> int -> char -> int -> int
    external compare :
      string -> string -> int = "caml_string_length_based_compare"[@@noalloc
                                                                    ]
    val capitalize_ascii : string -> string
    val capitalize_sub : string -> int -> string
    val uncapitalize_ascii : string -> string
    val get_1_2_3_4 : string -> off:int -> int -> int
    val unsafe_sub : string -> int -> int -> string
  end =
  struct
    let rec ends_aux s end_ j k =
      if k < 0
      then j + 1
      else
        if (String.unsafe_get s j) = (String.unsafe_get end_ k)
        then ends_aux s end_ (j - 1) (k - 1)
        else (-1)
    let ends_with_index s end_ =
      (let s_finish = (String.length s) - 1 in
       let s_beg = (String.length end_) - 1 in
       if s_beg > s_finish then (-1) else ends_aux s end_ s_finish s_beg : 
      int)[@@ocaml.doc
            " return an index which is minus when [s] does not \n    end with [beg]\n"]
    let rec unsafe_for_all_range s ~start  ~finish  p =
      (start > finish) ||
        ((p (String.unsafe_get s start)) &&
           (unsafe_for_all_range s ~start:(start + 1) ~finish p))
    let unsafe_is_sub ~sub  i s j ~len  =
      let rec check k =
        if k = len
        then true
        else
          ((String.unsafe_get sub (i + k)) = (String.unsafe_get s (j + k)))
            && (check (k + 1)) in
      ((j + len) <= (String.length s)) && (check 0)
    let find ?(start= 0)  ~sub  s =
      let exception Local_exit  in
        let n = String.length sub in
        let s_len = String.length s in
        let i = ref start in
        try
          while ((!i) + n) <= s_len do
            (if unsafe_is_sub ~sub 0 s (!i) ~len:n
             then raise_notrace Local_exit;
             incr i)
            done;
          (-1)
        with | Local_exit -> !i
    let rec index_rec_count s lim i c count =
      if i >= lim
      then (-1)
      else
        if (String.unsafe_get s i) = c
        then
          (if count = 1
           then i
           else index_rec_count s lim (i + 1) c (count - 1))
        else index_rec_count s lim (i + 1) c count
    let index_count s i c count =
      let lim = String.length s in
      if (i < 0) || ((i >= lim) || (count < 1))
      then
        invalid_arg
          ("index_count: ( " ^
             ((string_of_int i) ^ ("," ^ ((string_of_int count) ^ ")"))));
      index_rec_count s lim i c count
    let rec rindex_rec s i c =
      if i < 0
      then i
      else if (String.unsafe_get s i) = c then i else rindex_rec s (i - 1) c
    let rec rindex_rec_opt s i c =
      if i < 0
      then None
      else
        if (String.unsafe_get s i) = c
        then Some i
        else rindex_rec_opt s (i - 1) c
    let rec unsafe_no_char x ch i last_idx =
      (i > last_idx) ||
        (((String.unsafe_get x i) <> ch) &&
           (unsafe_no_char x ch (i + 1) last_idx))[@@ocaml.doc
                                                    " TODO: can be improved to return a positive integer instead "]
    let rec unsafe_no_char_idx x ch i last_idx =
      if i > last_idx
      then (-1)
      else
        if (String.unsafe_get x i) <> ch
        then unsafe_no_char_idx x ch (i + 1) last_idx
        else i
    external compare :
      string -> string -> int = "caml_string_length_based_compare"[@@noalloc
                                                                    ]
    let capitalize_ascii (s : string) =
      (if (String.length s) = 0
       then s
       else
         (let c = String.unsafe_get s 0 in
          if
            ((c >= 'a') && (c <= 'z')) ||
              (((c >= '\224') && (c <= '\246')) ||
                 ((c >= '\248') && (c <= '\254')))
          then
            let uc = Char.unsafe_chr ((Char.code c) - 32) in
            let bytes = Bytes.of_string s in
            (Bytes.unsafe_set bytes 0 uc; Bytes.unsafe_to_string bytes)
          else s) : string)
    let capitalize_sub (s : string) len =
      (let slen = String.length s in
       if (len < 0) || (len > slen)
       then invalid_arg "Ext_string.capitalize_sub"
       else
         if len = 0
         then ""
         else
           (let bytes = Bytes.create len in
            let uc =
              let c = String.unsafe_get s 0 in
              if
                ((c >= 'a') && (c <= 'z')) ||
                  (((c >= '\224') && (c <= '\246')) ||
                     ((c >= '\248') && (c <= '\254')))
              then Char.unsafe_chr ((Char.code c) - 32)
              else c in
            Bytes.unsafe_set bytes 0 uc;
            for i = 1 to len - 1 do
              Bytes.unsafe_set bytes i (String.unsafe_get s i)
            done;
            Bytes.unsafe_to_string bytes) : string)
    let uncapitalize_ascii = String.uncapitalize_ascii
    let get_int_1 (x : string) off = (Char.code (x.[off]) : int)
    let get_int_2 (x : string) off =
      ((Char.code (x.[off])) lor ((Char.code (x.[off + 1])) lsl 8) : 
      int)
    let get_int_3 (x : string) off =
      (((Char.code (x.[off])) lor ((Char.code (x.[off + 1])) lsl 8)) lor
         ((Char.code (x.[off + 2])) lsl 16) : int)
    let get_int_4 (x : string) off =
      ((((Char.code (x.[off])) lor ((Char.code (x.[off + 1])) lsl 8)) lor
          ((Char.code (x.[off + 2])) lsl 16))
         lor ((Char.code (x.[off + 3])) lsl 24) : int)
    let get_1_2_3_4 (x : string) ~off  len =
      (if len = 1
       then get_int_1 x off
       else
         if len = 2
         then get_int_2 x off
         else
           if len = 3
           then get_int_3 x off
           else if len = 4 then get_int_4 x off else assert false : int)
    let unsafe_sub x offs len =
      let b = Bytes.create len in
      Ext_bytes.unsafe_blit_string x offs b 0 len; Bytes.unsafe_to_string b
  end 
module Ext_string_array :
  sig val find_sorted : string array -> string -> int option end =
  struct
    let cmp = Ext_string.compare
    let rec binarySearchAux (arr : string array) (lo : int) (hi : int)
      (key : string) =
      (let mid = (lo + hi) / 2 in
       let midVal = Array.unsafe_get arr mid in
       let c = cmp key midVal in
       if c = 0
       then Some mid
       else
         if c < 0
         then
           (if hi = mid
            then
              let loVal = Array.unsafe_get arr lo in
              (if loVal = key then Some lo else None)
            else binarySearchAux arr lo mid key)
         else
           if lo = mid
           then
             (let hiVal = Array.unsafe_get arr hi in
              if hiVal = key then Some hi else None)
           else binarySearchAux arr mid hi key : _ option)
    let find_sorted sorted key =
      (let len = Array.length sorted in
       if len = 0
       then None
       else
         (let lo = Array.unsafe_get sorted 0 in
          let c = cmp key lo in
          if c < 0
          then None
          else
            (let hi = Array.unsafe_get sorted (len - 1) in
             let c2 = cmp key hi in
             if c2 > 0 then None else binarySearchAux sorted 0 (len - 1) key)) : 
      int option)
    let rec binarySearchAssoc (arr : (string * _) array) (lo : int)
      (hi : int) (key : string) =
      (let mid = (lo + hi) / 2 in
       let midVal = Array.unsafe_get arr mid in
       let c = cmp key (fst midVal) in
       if c = 0
       then Some (snd midVal)
       else
         if c < 0
         then
           (if hi = mid
            then
              let loVal = Array.unsafe_get arr lo in
              (if (fst loVal) = key then Some (snd loVal) else None)
            else binarySearchAssoc arr lo mid key)
         else
           if lo = mid
           then
             (let hiVal = Array.unsafe_get arr hi in
              if (fst hiVal) = key then Some (snd hiVal) else None)
           else binarySearchAssoc arr mid hi key : _ option)
  end 
module Literals :
  sig
    [@@@ocaml.text " callback actually, not exposed to user yet "]
    [@@@ocaml.text " nodejs "]
    val suffix_cmj : string
    val suffix_cmo : string
    val suffix_cmi : string
    val suffix_cmx : string
    val suffix_d : string
    val bsbuild_cache : string
    val ns_sep : string
  end =
  struct
    let suffix_cmj = ".cmj"
    let suffix_cmo = ".cmo"
    let suffix_cmi = ".cmi"
    let suffix_cmx = ".cmx"
    let suffix_d = ".d"
    let bsbuild_cache = ".bsbuild"
    let ns_sep = "-"
  end 
module Bsb_db_decode :
  sig
    type t
    type group =
      {
      modules: string array ;
      dir_length: int ;
      dir_info_offset: int ;
      module_info_offset: int }
    val read_build_cache : dir:string -> t
    type module_info = {
      case: bool ;
      dir_name: string }
    val find_opt : t -> int -> string -> module_info option
  end =
  struct
    let bsbuild_cache = Literals.bsbuild_cache
    type group =
      {
      modules: string array ;
      dir_length: int ;
      dir_info_offset: int ;
      module_info_offset: int }
    type t = (group array * string)
    type cursor = int ref
    let rec decode_internal (x : string) (offset : cursor) =
      let len = Ext_pervasives.parse_nat_of_string x offset in
      incr offset;
      (let first = decode_single x offset in
       if len = 1
       then [|first|]
       else
         (let result = Array.make len first in
          for i = 1 to len - 1 do
            Array.unsafe_set result i (decode_single x offset)
          done;
          result))
    and decode_single (x : string) (offset : cursor) =
      (let module_number = Ext_pervasives.parse_nat_of_string x offset in
       incr offset;
       (let modules = decode_modules x offset module_number in
        let dir_info_offset = !offset in
        let module_info_offset =
          (String.index_from x dir_info_offset '\n') + 1 in
        let dir_length = (Char.code (x.[module_info_offset])) - 48 in
        offset :=
          (((module_info_offset + 1) + (dir_length * module_number)) + 1);
        { modules; dir_info_offset; module_info_offset; dir_length }) : 
      group)
    and decode_modules (x : string) (offset : cursor) module_number =
      (let result = Array.make module_number "" in
       let last = ref (!offset) in
       let cur = ref (!offset) in
       let tasks = ref 0 in
       while (!tasks) <> module_number do
         (if (String.unsafe_get x (!cur)) = '\n'
          then
            (let offs = !last in
             let len = (!cur) - (!last) in
             Array.unsafe_set result (!tasks)
               (Ext_string.unsafe_sub x offs len);
             incr tasks;
             last := ((!cur) + 1));
          incr cur)
         done;
       offset := (!cur);
       result : string array)
    let read_build_cache ~dir  =
      (let all_content = Ext_io.load_file (Filename.concat dir bsbuild_cache) in
       ((decode_internal all_content (ref (Ext_digest.length + 1))),
         all_content) : t)
    type module_info = {
      case: bool ;
      dir_name: string }
    let find_opt ((sorteds, whole) : t) i (key : string) =
      (let group = sorteds.(i) in
       let i = Ext_string_array.find_sorted group.modules key in
       match i with
       | None -> None
       | Some count ->
           let encode_len = group.dir_length in
           let index =
             Ext_string.get_1_2_3_4 whole
               ~off:((group.module_info_offset + 1) + (count * encode_len))
               encode_len in
           let case = not ((index mod 2) = 0) in
           let ith = index lsr 1 in
           let dir_name_start =
             if ith = 0
             then group.dir_info_offset
             else
               (Ext_string.index_count whole group.dir_info_offset '\t' ith)
                 + 1 in
           let dir_name_finish = String.index_from whole dir_name_start '\t' in
           Some
             {
               case;
               dir_name =
                 (String.sub whole dir_name_start
                    (dir_name_finish - dir_name_start))
             } : module_info option)
  end 
module Ext_filename :
  sig
    [@@@ocaml.text
      " An extension module to calculate relative path follow node/npm style. \n    TODO : this short name will have to change upon renaming the file.\n"]
    val chop_extension_maybe : string -> string
    val new_extension : string -> string -> string
    val chop_all_extensions_maybe : string -> string
    val module_name : string -> string
    type module_info =
      {
      module_name: string [@dead "Ext_filename.module_info.module_name"];
      case: bool [@dead "Ext_filename.module_info.case"]}
  end =
  struct
    let is_dir_sep_unix c = c = '/'
    let is_dir_sep_win_cygwin c = (c = '/') || ((c = '\\') || (c = ':'))
    let is_dir_sep =
      if Sys.unix then is_dir_sep_unix else is_dir_sep_win_cygwin
    let chop_extension_maybe name =
      let rec search_dot i =
        if (i < 0) || (is_dir_sep (String.unsafe_get name i))
        then name
        else
          if (String.unsafe_get name i) = '.'
          then String.sub name 0 i
          else search_dot (i - 1) in
      search_dot ((String.length name) - 1)
    let chop_all_extensions_maybe name =
      let rec search_dot i last =
        if (i < 0) || (is_dir_sep (String.unsafe_get name i))
        then match last with | None -> name | Some i -> String.sub name 0 i
        else
          if (String.unsafe_get name i) = '.'
          then search_dot (i - 1) (Some i)
          else search_dot (i - 1) last in
      search_dot ((String.length name) - 1) None
    let new_extension name (ext : string) =
      let rec search_dot name i ext =
        if (i < 0) || (is_dir_sep (String.unsafe_get name i))
        then name ^ ext
        else
          if (String.unsafe_get name i) = '.'
          then
            (let ext_len = String.length ext in
             let buf = Bytes.create (i + ext_len) in
             Bytes.blit_string name 0 buf 0 i;
             Bytes.blit_string ext 0 buf i ext_len;
             Bytes.unsafe_to_string buf)
          else search_dot name (i - 1) ext in
      search_dot name ((String.length name) - 1) ext
    let module_name name =
      let rec search_dot i name =
        if i < 0
        then Ext_string.capitalize_ascii name
        else
          if (String.unsafe_get name i) = '.'
          then Ext_string.capitalize_sub name i
          else search_dot (i - 1) name in
      let name = Filename.basename name in
      let name_len = String.length name in search_dot (name_len - 1) name
      [@@ocaml.doc
        " TODO: improve efficiency\n   given a path, calcuate its module name \n   Note that `ocamlc.opt -c aa.xx.mli` gives `aa.xx.cmi`\n   we can not strip all extensions, otherwise\n   we can not tell the difference between \"x.cpp.ml\" \n   and \"x.ml\"\n"]
    type module_info = {
      module_name: string ;
      case: bool }
    let rec valid_module_name_aux name off len =
      if off >= len
      then true
      else
        (let c = String.unsafe_get name off in
         match c with
         | 'A'..'Z'|'a'..'z'|'0'..'9'|'_'|'\'' ->
             valid_module_name_aux name (off + 1) len
         | _ -> false)
    type state =
      | Invalid 
      | Upper 
      | Lower 
    let valid_module_name name len =
      if len = 0
      then Invalid
      else
        (let c = String.unsafe_get name 0 in
         match c with
         | 'A'..'Z' ->
             if valid_module_name_aux name 1 len then Upper else Invalid
         | 'a'..'z' ->
             if valid_module_name_aux name 1 len then Lower else Invalid
         | _ -> Invalid)
  end 
module Ext_namespace_encode :
  sig
    val make : ?ns:string -> string -> string[@@ocaml.doc
                                               " [make ~ns:\"Ns\" \"a\" ]\n    A typical example would return \"a-Ns\"\n    Note the namespace comes from the output of [namespace_of_package_name]\n"]
  end =
  struct
    let make ?ns  cunit =
      match ns with
      | None -> cunit
      | Some ns -> cunit ^ (Literals.ns_sep ^ ns)
  end 
module Bsb_helper_depfile_gen :
  sig
    type kind =
      | Js 
      | Bytecode [@dead "Bsb_helper_depfile_gen.kind.Bytecode"]
      | Native [@dead "Bsb_helper_depfile_gen.kind.Native"]
    val emit_d :
      kind -> Bsb_dir_index.t -> string option -> string -> string -> unit
  end =
  struct
    let dep_lit = " : "
    let write_buf name buf =
      let oc = open_out_bin name in
      Ext_buffer.output_buffer oc buf; close_out oc
    let load_file name (buf : Ext_buffer.t) =
      (let len = Ext_buffer.length buf in
       let ic = open_in_bin name in
       let n = in_channel_length ic in
       if n <> len
       then (close_in ic; write_buf name buf)
       else
         (let holder = really_input_string ic n in
          close_in ic;
          if Ext_buffer.not_equal buf holder then write_buf name buf) : 
      unit)
    let write_file name (buf : Ext_buffer.t) =
      if Sys.file_exists name then load_file name buf else write_buf name buf
    let extract_dep_raw_string (fn : string) =
      (let ic = open_in_bin fn in
       let size = input_binary_int ic in
       let s = really_input_string ic size in close_in ic; s : string)
    let magic_sep_char = '\n'
    [@@@ocaml.text
      " Please refer to {!Binary_ast} for encoding format, we move it here \n    mostly for cutting the dependency so that [bsb_helper.exe] does\n    not depend on compler-libs\n"]
    type kind =
      | Js 
      | Bytecode 
      | Native 
    let output_file (buf : Ext_buffer.t) source namespace =
      Ext_buffer.add_string buf
        (Ext_namespace_encode.make ?ns:namespace source)
    let oc_cmi buf namespace source =
      Ext_buffer.add_char buf ' ';
      output_file buf source namespace;
      Ext_buffer.add_string buf Literals.suffix_cmi[@@ocaml.doc
                                                     " for bucklescript artifacts \n    [lhs_suffix] is [.cmj]\n    [rhs_suffix] \n    is [.cmj] if it has [ml] (in this case does not care about mli or not)\n    is [.cmi] if it has [mli]\n"]
    let find_module db dependent_module is_not_lib_dir
      (index : Bsb_dir_index.t) =
      let opt = Bsb_db_decode.find_opt db 0 dependent_module in
      match opt with
      | Some _ -> opt
      | None ->
          if is_not_lib_dir
          then Bsb_db_decode.find_opt db (index :> int) dependent_module
          else None
    let oc_impl (mlast : string) (index : Bsb_dir_index.t)
      (db : Bsb_db_decode.t) (namespace : string option) (buf : Ext_buffer.t)
      (lhs_suffix : string) (rhs_suffix : string) =
      let has_deps = ref false in
      let cur_module_name = Ext_filename.module_name mlast in
      let at_most_once : unit lazy_t =
        lazy
          (has_deps := true;
           output_file buf (Ext_filename.chop_extension_maybe mlast)
             namespace;
           Ext_buffer.add_string buf lhs_suffix;
           Ext_buffer.add_string buf dep_lit) in
      (match namespace with
       | None -> ()
       | Some ns ->
           (Lazy.force at_most_once;
            Ext_buffer.add_string buf ns;
            Ext_buffer.add_string buf Literals.suffix_cmi));
      (let is_not_lib_dir = not (Bsb_dir_index.is_lib_dir index) in
       let s = extract_dep_raw_string mlast in
       let offset = ref 1 in
       let size = String.length s in
       while (!offset) < size do
         (let next_tab = String.index_from s (!offset) magic_sep_char in
          let dependent_module =
            String.sub s (!offset) (next_tab - (!offset)) in
          if dependent_module = cur_module_name
          then
            (prerr_endline
               ("FAILED: " ^ (cur_module_name ^ " has a self cycle"));
             exit 2);
          (match find_module db dependent_module is_not_lib_dir index with
           | None -> ()
           | Some { dir_name; case } ->
               (Lazy.force at_most_once;
                (let source =
                   Filename.concat dir_name
                     (if case
                      then dependent_module
                      else Ext_string.uncapitalize_ascii dependent_module) in
                 Ext_buffer.add_char buf ' ';
                 output_file buf source namespace;
                 Ext_buffer.add_string buf rhs_suffix;
                 oc_cmi buf namespace source)));
          offset := (next_tab + 1))
         done;
       if !has_deps then Ext_buffer.add_char buf '\n')
    let oc_intf mliast (index : Bsb_dir_index.t) (db : Bsb_db_decode.t)
      (namespace : string option) (buf : Ext_buffer.t) =
      (let has_deps = ref false in
       let at_most_once : unit lazy_t =
         lazy
           (has_deps := true;
            output_file buf (Ext_filename.chop_all_extensions_maybe mliast)
              namespace;
            Ext_buffer.add_string buf Literals.suffix_cmi;
            Ext_buffer.add_string buf dep_lit) in
       (match namespace with
        | None -> ()
        | Some ns ->
            (Lazy.force at_most_once;
             Ext_buffer.add_string buf ns;
             Ext_buffer.add_string buf Literals.suffix_cmi));
       (let cur_module_name = Ext_filename.module_name mliast in
        let is_not_lib_dir = not (Bsb_dir_index.is_lib_dir index) in
        let s = extract_dep_raw_string mliast in
        let offset = ref 1 in
        let size = String.length s in
        while (!offset) < size do
          (let next_tab = String.index_from s (!offset) magic_sep_char in
           let dependent_module =
             String.sub s (!offset) (next_tab - (!offset)) in
           if dependent_module = cur_module_name
           then
             (prerr_endline
                ("FAILED: " ^ (cur_module_name ^ " has a self cycle"));
              exit 2);
           (match find_module db dependent_module is_not_lib_dir index with
            | None -> ()
            | Some { dir_name; case } ->
                (Lazy.force at_most_once;
                 oc_cmi buf namespace
                   (Filename.concat dir_name
                      (if case
                       then dependent_module
                       else Ext_string.uncapitalize_ascii dependent_module))));
           offset := (next_tab + 1))
          done;
        if !has_deps then Ext_buffer.add_char buf '\n') : unit)[@@ocaml.doc
                                                                 " Note since dependent file is [mli], it only depends on \n    [.cmi] file\n"]
    let emit_d compilation_kind (index : Bsb_dir_index.t)
      (namespace : string option) (mlast : string) (mliast : string) =
      let data =
        Bsb_db_decode.read_build_cache ~dir:Filename.current_dir_name in
      let buf = Ext_buffer.create 2048 in
      let filename = Ext_filename.new_extension mlast Literals.suffix_d in
      let (lhs_suffix, rhs_suffix) =
        match compilation_kind with
        | Js -> (Literals.suffix_cmj, Literals.suffix_cmj)
        | Bytecode -> (Literals.suffix_cmo, Literals.suffix_cmo)
        | Native -> (Literals.suffix_cmx, Literals.suffix_cmx) in
      oc_impl mlast index data namespace buf lhs_suffix rhs_suffix;
      if mliast <> "" then oc_intf mliast index data namespace buf;
      write_file filename buf
  end 
module Bsb_helper_main :
  sig
    [@@@ocaml.text
      " Used to generate .d file, for example \n  {[\n    bsb_helper.exe -g 0 -MD  src/hi/hello.ml\n  ]}\n  It will read the cache file and generate the corresponding\n     [.d] file. This [.d] file will be used as attribute [depfile]\n  whether we use namespace or not, the filename of [.mlast], [.d] \n  should be kept the same, we only need change the name of [.cm*]\n  and the contents of filename in [.d]\n "]
  end =
  struct
    let compilation_kind = ref Bsb_helper_depfile_gen.Js
    let hash : string ref = ref ""
    let batch_files = ref []
    let collect_file name = batch_files := (name :: (!batch_files))
    let dev_group = ref 0
    let namespace = ref None
    let anonymous filename = collect_file filename
    let usage = "Usage: bsb_helper.exe [options] \nOptions are:"
    let () =
      Bsb_helper_arg.parse_exn
        [("-g", (Set_int dev_group), " Set the dev group (default to be 0)");
        ("-bs-ns", (String ((fun s -> namespace := (Some s)))),
          " Set namespace");
        ("-hash", (Set_string hash), " Set hash(internal)")] anonymous usage;
      (match !batch_files with
       | x::[] ->
           Bsb_helper_depfile_gen.emit_d (!compilation_kind)
             (Bsb_dir_index.of_int (!dev_group)) (!namespace) x ""
       | y::x::[] ->
           Bsb_helper_depfile_gen.emit_d (!compilation_kind)
             (Bsb_dir_index.of_int (!dev_group)) (!namespace) x y
       | _ -> ())
  end 
