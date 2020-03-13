module Bsb_dir_index :
  sig
    type t = private int[@@ocaml.doc
                          " Used to index [.bsbuildcache] may not be needed if we flatten dev \n  into  a single group\n"]
    val lib_dir_index : t
    val is_lib_dir : t -> bool
    val get_dev_index : unit -> t
    val of_int : int -> t
    val get_current_number_of_dev_groups : unit -> int
    val string_of_bsb_dev_include : t -> string
    val reset : unit -> unit[@@ocaml.doc
                              " TODO: Need reset\n   when generating each ninja file to provide stronger guarantee. \n   Here we get a weak guarantee because only dev group is \n  inside the toplevel project\n   "]
  end =
  struct
    type t = int
    external of_int : int -> t = "%identity"[@@ocaml.doc
                                              " \n   0 : lib \n   1 : dev 1 \n   2 : dev 2 \n"]
    let lib_dir_index = 0
    let is_lib_dir x = x = lib_dir_index
    let dir_index = ref 0
    let get_dev_index () = incr dir_index; !dir_index
    let get_current_number_of_dev_groups () = !dir_index
    let bsc_group_1_includes = "bsc_group_1_includes"[@@ocaml.doc
                                                       " bsb generate pre-defined variables [bsc_group_i_includes]\n  for each rule, there is variable [bsc_extra_excludes]\n  [g_dev_incls] are for app test etc\n  it will be like\n  {[\n    g_dev_incls = ${bsc_group_1_includes}\n  ]}\n  where [bsc_group_1_includes] will be pre-calcuated\n"]
    let bsc_group_2_includes = "bsc_group_2_includes"
    let bsc_group_3_includes = "bsc_group_3_includes"
    let bsc_group_4_includes = "bsc_group_4_includes"
    let string_of_bsb_dev_include i =
      match i with
      | 1 -> bsc_group_1_includes
      | 2 -> bsc_group_2_includes
      | 3 -> bsc_group_3_includes
      | 4 -> bsc_group_4_includes
      | _ -> "bsc_group_" ^ ((string_of_int i) ^ "_includes")
    let reset () = dir_index := 0
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
    val is_empty : t -> bool
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
    val digest : t -> Digest.t
    val not_equal : t -> string -> bool
    val add_int_1 : t -> int -> unit
    val add_int_2 : t -> int -> unit
    val add_int_3 : t -> int -> unit
    val add_int_4 : t -> int -> unit
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
    let is_empty b = b.position = 0
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
    let digest b = unsafe_string b.buffer 0 b.position
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
    let add_int_1 (b : t) (x : int) =
      let c = Char.unsafe_chr (x land 0xff) in
      let pos = b.position in
      if pos >= b.length then resize b 1;
      Bytes.unsafe_set b.buffer pos c;
      b.position <- (pos + 1)[@@ocaml.doc
                               "\n  It could be one byte, two bytes, three bytes and four bytes \n  TODO: inline for better performance\n"]
    let add_int_2 (b : t) (x : int) =
      let c1 = Char.unsafe_chr (x land 0xff) in
      let c2 = Char.unsafe_chr ((x lsr 8) land 0xff) in
      let pos = b.position in
      if (pos + 1) >= b.length then resize b 2;
      (let b_buffer = b.buffer in
       Bytes.unsafe_set b_buffer pos c1;
       Bytes.unsafe_set b_buffer (pos + 1) c2;
       b.position <- (pos + 2))
    let add_int_3 (b : t) (x : int) =
      let c1 = Char.unsafe_chr (x land 0xff) in
      let c2 = Char.unsafe_chr ((x lsr 8) land 0xff) in
      let c3 = Char.unsafe_chr ((x lsr 16) land 0xff) in
      let pos = b.position in
      if (pos + 2) >= b.length then resize b 3;
      (let b_buffer = b.buffer in
       Bytes.unsafe_set b_buffer pos c1;
       Bytes.unsafe_set b_buffer (pos + 1) c2;
       Bytes.unsafe_set b_buffer (pos + 2) c3;
       b.position <- (pos + 3))
    let add_int_4 (b : t) (x : int) =
      let c1 = Char.unsafe_chr (x land 0xff) in
      let c2 = Char.unsafe_chr ((x lsr 8) land 0xff) in
      let c3 = Char.unsafe_chr ((x lsr 16) land 0xff) in
      let c4 = Char.unsafe_chr ((x lsr 24) land 0xff) in
      let pos = b.position in
      if (pos + 3) >= b.length then resize b 4;
      (let b_buffer = b.buffer in
       Bytes.unsafe_set b_buffer pos c1;
       Bytes.unsafe_set b_buffer (pos + 1) c2;
       Bytes.unsafe_set b_buffer (pos + 2) c3;
       Bytes.unsafe_set b_buffer (pos + 3) c4;
       b.position <- (pos + 4))
  end 
module Ext_list :
  sig
    val map : 'a list -> ('a -> 'b) -> 'b list
    val map_combine : 'a list -> 'b list -> ('a -> 'c) -> ('c * 'b) list
    val has_string : string list -> string -> bool
    val map_split_opt :
      'a list -> ('a -> ('b option * 'c option)) -> ('b list * 'c list)
    val mapi : 'a list -> (int -> 'a -> 'b) -> 'b list
    val map_snd : ('a * 'b) list -> ('b -> 'c) -> ('a * 'c) list
    val map_last : 'a list -> (bool -> 'a -> 'b) -> 'b list[@@ocaml.doc
                                                             " [map_last f xs ]\n    will pass [true] to [f] for the last element, \n    [false] otherwise. \n    For empty list, it returns empty\n"]
    val last : 'a list -> 'a[@@ocaml.doc
                              " [last l]\n    return the last element\n    raise if the list is empty\n"]
    val append : 'a list -> 'a list -> 'a list
    val append_one : 'a list -> 'a -> 'a list
    val map_append : 'b list -> 'a list -> ('b -> 'a) -> 'a list
    val fold_right : 'a list -> 'b -> ('a -> 'b -> 'b) -> 'b
    val fold_right2 :
      'a list -> 'b list -> 'c -> ('a -> 'b -> 'c -> 'c) -> 'c
    val map2 : 'a list -> 'b list -> ('a -> 'b -> 'c) -> 'c list
    val fold_left_with_offset :
      'a list -> 'acc -> int -> ('a -> 'acc -> int -> 'acc) -> 'acc
    val filter_map : 'a list -> ('a -> 'b option) -> 'b list[@@ocaml.doc
                                                              " @unused "]
    val exclude : 'a list -> ('a -> bool) -> 'a list[@@ocaml.doc
                                                      " [exclude p l] is the opposite of [filter p l] "]
    val exclude_with_val : 'a list -> ('a -> bool) -> 'a list option[@@ocaml.doc
                                                                    " [excludes p l]\n    return a tuple [excluded,newl]\n    where [exluded] is true indicates that at least one  \n    element is removed,[newl] is the new list where all [p x] for [x] is false\n\n"]
    val same_length : 'a list -> 'b list -> bool
    val init : int -> (int -> 'a) -> 'a list
    val split_at : 'a list -> int -> ('a list * 'a list)[@@ocaml.doc
                                                          " [split_at n l]\n    will split [l] into two lists [a,b], [a] will be of length [n], \n    otherwise, it will raise\n"]
    val split_at_last : 'a list -> ('a list * 'a)[@@ocaml.doc
                                                   " [split_at_last l]\n    It is equivalent to [split_at (List.length l - 1) l ]\n"]
    val filter_mapi : 'a list -> ('a -> int -> 'b option) -> 'b list
    val filter_map2 :
      'a list -> 'b list -> ('a -> 'b -> 'c option) -> 'c list
    val length_compare : 'a list -> int -> [ `Gt  | `Eq  | `Lt ]
    val length_ge : 'a list -> int -> bool
    [@@@ocaml.text
      "\n\n   {[length xs = length ys + n ]}\n   input n should be positive \n   TODO: input checking\n"]
    val length_larger_than_n : 'a list -> 'a list -> int -> bool
    val rev_map_append : 'a list -> 'b list -> ('a -> 'b) -> 'b list[@@ocaml.doc
                                                                    "\n   [rev_map_append f l1 l2]\n   [map f l1] and reverse it to append [l2]\n   This weird semantics is due to it is the most efficient operation\n   we can do\n"]
    val flat_map : 'a list -> ('a -> 'b list) -> 'b list
    val flat_map_append : 'a list -> 'b list -> ('a -> 'b list) -> 'b list
    val stable_group : 'a list -> ('a -> 'a -> bool) -> 'a list list[@@ocaml.doc
                                                                    "\n    [stable_group eq lst]\n    Example:\n    Input:\n   {[\n     stable_group (=) [1;2;3;4;3]\n   ]}\n    Output:\n   {[\n     [[1];[2];[4];[3;3]]\n   ]}\n    TODO: this is O(n^2) behavior \n    which could be improved later\n"]
    val drop : 'a list -> int -> 'a list[@@ocaml.doc
                                          " [drop n list]\n    raise when [n] is negative\n    raise when list's length is less than [n]\n"]
    val find_first : 'a list -> ('a -> bool) -> 'a option
    val find_first_not : 'a list -> ('a -> bool) -> 'a option[@@ocaml.doc
                                                               " [find_first_not p lst ]\n    if all elements in [lst] pass, return [None] \n    otherwise return the first element [e] as [Some e] which\n    fails the predicate\n"]
    [@@@ocaml.text
      " [find_opt f l] returns [None] if all return [None],  \n    otherwise returns the first one. \n"]
    val find_opt : 'a list -> ('a -> 'b option) -> 'b option
    val find_def : 'a list -> ('a -> 'b option) -> 'b -> 'b
    val rev_iter : 'a list -> ('a -> unit) -> unit
    val iter : 'a list -> ('a -> unit) -> unit
    val for_all : 'a list -> ('a -> bool) -> bool
    val for_all_snd : ('a * 'b) list -> ('b -> bool) -> bool
    val for_all2_no_exn : 'a list -> 'b list -> ('a -> 'b -> bool) -> bool
    [@@ocaml.doc
      " [for_all2_no_exn p xs ys]\n    return [true] if all satisfied,\n    [false] otherwise or length not equal\n"]
    val split_map : 'a list -> ('a -> ('b * 'c)) -> ('b list * 'c list)
    [@@ocaml.doc " [f] is applied follow the list order "]
    val reduce_from_left : 'a list -> ('a -> 'a -> 'a) -> 'a[@@ocaml.doc
                                                              " [fn] is applied from left to right "]
    val sort_via_array : 'a list -> ('a -> 'a -> int) -> 'a list
    val assoc_by_string : (string * 'a) list -> string -> 'a option -> 'a
    [@@ocaml.doc
      " [assoc_by_string default key lst]\n    if  [key] is found in the list  return that val,\n    other unbox the [default], \n    otherwise [assert false ]\n"]
    val assoc_by_int : (int * 'a) list -> int -> 'a option -> 'a
    val nth_opt : 'a list -> int -> 'a option
    val iter_snd : ('a * 'b) list -> ('b -> unit) -> unit
    val iter_fst : ('a * 'b) list -> ('a -> unit) -> unit
    val exists : 'a list -> ('a -> bool) -> bool
    val exists_fst : ('a * 'b) list -> ('a -> bool) -> bool
    val exists_snd : ('a * 'b) list -> ('b -> bool) -> bool
    val concat_append : 'a list list -> 'a list -> 'a list
    val fold_left2 : 'a list -> 'b list -> 'c -> ('a -> 'b -> 'c -> 'c) -> 'c
    val fold_left : 'a list -> 'b -> ('b -> 'a -> 'b) -> 'b
    val singleton_exn : 'a list -> 'a
    val mem_string : string list -> string -> bool
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
    let mapi lst f = mapi_aux lst 0 f
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
    let append_one l1 x = append_aux l1 [x]
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
    let init n f =
      match n with
      | 0 -> []
      | 1 -> let a0 = f 0 in [a0]
      | 2 -> let a0 = f 0 in let a1 = f 1 in [a0; a1]
      | 3 -> let a0 = f 0 in let a1 = f 1 in let a2 = f 2 in [a0; a1; a2]
      | 4 ->
          let a0 = f 0 in
          let a1 = f 1 in let a2 = f 2 in let a3 = f 3 in [a0; a1; a2; a3]
      | 5 ->
          let a0 = f 0 in
          let a1 = f 1 in
          let a2 = f 2 in
          let a3 = f 3 in let a4 = f 4 in [a0; a1; a2; a3; a4]
      | _ -> Array.to_list (Array.init n f)
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
    let split_at l n = small_split_at n [] l
    let rec split_at_last_aux acc x =
      match x with
      | [] -> invalid_arg "Ext_list.split_at_last"
      | x::[] -> ((rev acc), x)
      | y0::ys -> split_at_last_aux (y0 :: acc) ys
    let split_at_last (x : 'a list) =
      match x with
      | [] -> invalid_arg "Ext_list.split_at_last"
      | a0::[] -> ([], a0)
      | a0::a1::[] -> ([a0], a1)
      | a0::a1::a2::[] -> ([a0; a1], a2)
      | a0::a1::a2::a3::[] -> ([a0; a1; a2], a3)
      | a0::a1::a2::a3::a4::[] -> ([a0; a1; a2; a3], a4)
      | a0::a1::a2::a3::a4::rest ->
          let (rev, last) = split_at_last_aux [] rest in
          ((a0 :: a1 :: a2 :: a3 :: a4 :: rev), last)
    let filter_mapi xs f =
      let rec aux i xs =
        match xs with
        | [] -> []
        | y::ys ->
            (match f y i with
             | None -> aux (i + 1) ys
             | Some z -> z :: (aux (i + 1) ys)) in
      aux 0 xs[@@ocaml.doc
                "\n   can not do loop unroll due to state combination\n"]
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
    let flat_map lx f = flat_map_aux f [] [] lx
    let flat_map_append lx append f = flat_map_aux f [] append lx
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
    let stable_group lst eq = (group eq lst) |> rev
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
    let sort_via_array lst cmp =
      let arr = Array.of_list lst in Array.sort cmp arr; Array.to_list arr
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
    let nth_opt l n = if n < 0 then None else nth_aux l n
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
    let reduce_from_left lst fn =
      match lst with
      | first::rest -> fold_left rest first fn
      | _ -> invalid_arg "Ext_list.reduce_from_left"
    let rec fold_left2 l1 l2 accu f =
      match (l1, l2) with
      | ([], []) -> accu
      | (a1::l1, a2::l2) -> fold_left2 l1 l2 (f a1 a2 accu) f
      | (_, _) -> invalid_arg "Ext_list.fold_left2"
    let singleton_exn xs = match xs with | x::[] -> x | _ -> assert false
    let rec mem_string (xs : string list) (x : string) =
      match xs with | [] -> false | a::l -> (a = x) || (mem_string l x)
  end 
module Bsb_helper_arg :
  sig
    type spec =
      | Unit of (unit -> unit) 
      | Set of bool ref 
      | String of (string -> unit) 
      | Set_string of string ref 
      | Int of (int -> unit) 
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
module Ext_digest : sig val length : int val hex_length : int end =
  struct let length = 16
         let hex_length = 32 end 
module Ext_pervasives :
  sig
    [@@@ocaml.text
      " Extension to standard library [Pervavives] module, safe to open \n  "]
    external reraise : exn -> 'a = "%reraise"
    val finally : 'a -> clean:('a -> 'c) -> ('a -> 'b) -> 'b
    val with_file_as_chan : string -> (out_channel -> 'a) -> 'a
    [@@@ocaml.text
      " Copied from {!Btype.hash_variant}:\n    need sync up and add test case\n "]
    val nat_of_string_exn : string -> int
    val parse_nat_of_string : string -> int ref -> int
  end =
  struct
    external reraise : exn -> 'a = "%reraise"
    let finally v ~clean:action  f =
      match f v with
      | exception e -> (action v; reraise e)
      | e -> (action v; e)
    let with_file_as_chan filename f =
      finally (open_out_bin filename) ~clean:close_out f
    let rec int_of_string_aux s acc off len =
      if off >= len
      then acc
      else
        (let d = (Char.code (String.unsafe_get s off)) - 48 in
         if (d >= 0) && (d <= 9)
         then int_of_string_aux s ((10 * acc) + d) (off + 1) len
         else (-1))
    let nat_of_string_exn (s : string) =
      let acc = int_of_string_aux s 0 0 (String.length s) in
      if acc < 0 then invalid_arg s else acc
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
module Ext_io :
  sig
    val load_file : string -> string
    val rev_lines_of_file : string -> string list
    val rev_lines_of_chann : in_channel -> string list
    val write_file : string -> string -> unit
  end =
  struct
    let load_file f =
      Ext_pervasives.finally (open_in_bin f) ~clean:close_in
        (fun ic ->
           let n = in_channel_length ic in
           let s = Bytes.create n in
           really_input ic s 0 n; Bytes.unsafe_to_string s)[@@ocaml.doc
                                                             " on 32 bit , there are 16M limitation "]
    let rev_lines_of_chann chan =
      let rec loop acc chan =
        match input_line chan with
        | line -> loop (line :: acc) chan
        | exception End_of_file -> (close_in chan; acc) in
      loop [] chan
    let rev_lines_of_file file =
      Ext_pervasives.finally ~clean:close_in (open_in_bin file)
        rev_lines_of_chann
    let write_file f content =
      Ext_pervasives.finally ~clean:close_out (open_out_bin f)
        (fun oc -> output_string oc content)
  end 
module Ext_string :
  sig
    [@@@ocaml.text
      " Extension to the standard library [String] module, fixed some bugs like\n    avoiding locale sensitivity "]
    val split_by :
      ?keep_empty:bool -> (char -> bool) -> string -> string list[@@ocaml.doc
                                                                   " default is false "]
    val trim : string -> string[@@ocaml.doc
                                 " remove whitespace letters ('\\t', '\\n', ' ') on both side"]
    val split : ?keep_empty:bool -> string -> char -> string list[@@ocaml.doc
                                                                   " default is false "]
    val quick_split_by_ws : string -> string list[@@ocaml.doc
                                                   " split by space chars for quick scripting "]
    val starts_with : string -> string -> bool
    val ends_with_index : string -> string -> int[@@ocaml.doc
                                                   "\n   return [-1] when not found, the returned index is useful \n   see [ends_with_then_chop]\n"]
    val ends_with : string -> string -> bool
    val ends_with_then_chop : string -> string -> string option[@@ocaml.doc
                                                                 "\n  [ends_with_then_chop name ext]\n  @example:\n   {[\n     ends_with_then_chop \"a.cmj\" \".cmj\"\n     \"a\"\n   ]}\n   This is useful in controlled or file case sensitve system\n"]
    val for_all_from : string -> int -> (char -> bool) -> bool[@@ocaml.doc
                                                                "\n  [for_all_from  s start p]\n  if [start] is negative, it raises,\n  if [start] is too large, it returns true\n"]
    val for_all : string -> (char -> bool) -> bool
    val is_empty : string -> bool
    val repeat : int -> string -> string
    val equal : string -> string -> bool
    [@@@ocaml.text
      "\n  [extract_until s cursor sep]\n   When [sep] not found, the cursor is updated to -1,\n   otherwise cursor is increased to 1 + [sep_position]\n   User can not determine whether it is found or not by\n   telling the return string is empty since \n   \"\\n\\n\" would result in an empty string too.\n"]
    val index_count : string -> int -> char -> int -> int
    val find : ?start:int -> sub:string -> string -> int[@@ocaml.doc
                                                          "\n  [find ~start ~sub s]\n  returns [-1] if not found\n"]
    val contain_substring : string -> string -> bool
    val non_overlap_count : sub:string -> string -> int
    val rfind : sub:string -> string -> int
    val tail_from : string -> int -> string[@@ocaml.doc
                                             " [tail_from s 1]\n  return a substring from offset 1 (inclusive)\n"]
    val rindex_neg : string -> char -> int[@@ocaml.doc
                                            " returns negative number if not found "]
    val rindex_opt : string -> char -> int option
    val no_char : string -> char -> int -> int -> bool
    val no_slash : string -> bool
    val no_slash_idx : string -> int[@@ocaml.doc
                                      " return negative means no slash, otherwise [i] means the place for first slash "]
    val no_slash_idx_from : string -> int -> int
    val replace_slash_backward : string -> string[@@ocaml.doc
                                                   " if no conversion happens, reference equality holds "]
    val replace_backward_slash : string -> string[@@ocaml.doc
                                                   " if no conversion happens, reference equality holds "]
    val empty : string
    external compare :
      string -> string -> int = "caml_string_length_based_compare"[@@noalloc
                                                                    ]
    val single_space : string
    val concat3 : string -> string -> string -> string
    val concat4 : string -> string -> string -> string -> string
    val concat5 : string -> string -> string -> string -> string -> string
    val inter2 : string -> string -> string
    val inter3 : string -> string -> string -> string
    val inter4 : string -> string -> string -> string -> string
    val concat_array : string -> string array -> string
    val single_colon : string
    val parent_dir_lit : string
    val current_dir_lit : string
    val capitalize_ascii : string -> string
    val capitalize_sub : string -> int -> string
    val uncapitalize_ascii : string -> string
    val lowercase_ascii : string -> string
    val get_int_1 : string -> int -> int[@@ocaml.doc
                                          " Play parity to {!Ext_buffer.add_int_1} "]
    val get_int_2 : string -> int -> int
    val get_int_3 : string -> int -> int
    val get_int_4 : string -> int -> int
    val get_1_2_3_4 : string -> off:int -> int -> int
    val unsafe_sub : string -> int -> int -> string
  end =
  struct
    let split_by ?(keep_empty= false)  is_delim str =
      let len = String.length str in
      let rec loop acc last_pos pos =
        if pos = (-1)
        then
          (if (last_pos = 0) && (not keep_empty)
           then acc
           else (String.sub str 0 last_pos) :: acc)
        else
          if is_delim (str.[pos])
          then
            (let new_len = (last_pos - pos) - 1 in
             if (new_len <> 0) || keep_empty
             then
               let v = String.sub str (pos + 1) new_len in
               loop (v :: acc) pos (pos - 1)
             else loop acc pos (pos - 1))
          else loop acc last_pos (pos - 1) in
      loop [] len (len - 1)
    let trim s =
      let i = ref 0 in
      let j = String.length s in
      while
        ((!i) < j) &&
          ((let u = String.unsafe_get s (!i) in
            (u = '\t') || ((u = '\n') || (u = ' '))))
        do incr i done;
      (let k = ref (j - 1) in
       while
         ((!k) >= (!i)) &&
           ((let u = String.unsafe_get s (!k) in
             (u = '\t') || ((u = '\n') || (u = ' '))))
         do decr k done;
       String.sub s (!i) (((!k) - (!i)) + 1))
    let split ?keep_empty  str on =
      if str = ""
      then []
      else split_by ?keep_empty (fun x -> (x : char) = on) str
    let quick_split_by_ws str =
      (split_by ~keep_empty:false
         (fun x -> (x = '\t') || ((x = '\n') || (x = ' '))) str : string list)
    let starts_with s beg =
      let beg_len = String.length beg in
      let s_len = String.length s in
      (beg_len <= s_len) &&
        (let i = ref 0 in
         while
           ((!i) < beg_len) &&
             ((String.unsafe_get s (!i)) = (String.unsafe_get beg (!i)))
           do incr i done;
         (!i) = beg_len)
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
    let ends_with s end_ = (ends_with_index s end_) >= 0
    let ends_with_then_chop s beg =
      let i = ends_with_index s beg in
      if i >= 0 then Some (String.sub s 0 i) else None
    let rec unsafe_for_all_range s ~start  ~finish  p =
      (start > finish) ||
        ((p (String.unsafe_get s start)) &&
           (unsafe_for_all_range s ~start:(start + 1) ~finish p))
    let for_all_from s start p =
      let len = String.length s in
      if start < 0
      then invalid_arg "Ext_string.for_all_from"
      else unsafe_for_all_range s ~start ~finish:(len - 1) p
    let for_all s (p : char -> bool) =
      unsafe_for_all_range s ~start:0 ~finish:((String.length s) - 1) p
    let is_empty s = (String.length s) = 0
    let repeat n s =
      let len = String.length s in
      let res = Bytes.create (n * len) in
      for i = 0 to pred n do String.blit s 0 res (i * len) len done;
      Bytes.to_string res
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
    let contain_substring s sub = (find s ~sub) >= 0
    let non_overlap_count ~sub  s =
      let sub_len = String.length sub in
      let rec aux acc off =
        let i = find ~start:off ~sub s in
        if i < 0 then acc else aux (acc + 1) (i + sub_len) in
      if (String.length sub) = 0
      then invalid_arg "Ext_string.non_overlap_count"
      else aux 0 0[@@ocaml.doc
                    " TODO: optimize \n    avoid nonterminating when string is empty \n"]
    let rfind ~sub  s =
      let exception Local_exit  in
        let n = String.length sub in
        let i = ref ((String.length s) - n) in
        try
          while (!i) >= 0 do
            (if unsafe_is_sub ~sub 0 s (!i) ~len:n
             then raise_notrace Local_exit;
             decr i)
            done;
          (-1)
        with | Local_exit -> !i
    let tail_from s x =
      let len = String.length s in
      if x > len
      then
        invalid_arg
          ("Ext_string.tail_from " ^ (s ^ (" : " ^ (string_of_int x))))
      else String.sub s x (len - x)
    let equal (x : string) y = x = y
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
    let rindex_neg s c = rindex_rec s ((String.length s) - 1) c
    let rindex_opt s c = rindex_rec_opt s ((String.length s) - 1) c
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
    let no_char x ch i len =
      (let str_len = String.length x in
       if (i < 0) || ((i >= str_len) || (len >= str_len))
       then invalid_arg "Ext_string.no_char"
       else unsafe_no_char x ch i len : bool)
    let no_slash x = unsafe_no_char x '/' 0 ((String.length x) - 1)
    let no_slash_idx x = unsafe_no_char_idx x '/' 0 ((String.length x) - 1)
    let no_slash_idx_from x from =
      let last_idx = (String.length x) - 1 in
      assert (from >= 0); unsafe_no_char_idx x '/' from last_idx
    let replace_slash_backward (x : string) =
      let len = String.length x in
      if unsafe_no_char x '/' 0 (len - 1)
      then x
      else String.map (function | '/' -> '\\' | x -> x) x
    let replace_backward_slash (x : string) =
      let len = String.length x in
      if unsafe_no_char x '\\' 0 (len - 1)
      then x
      else String.map (function | '\\' -> '/' | x -> x) x
    let empty = ""
    external compare :
      string -> string -> int = "caml_string_length_based_compare"[@@noalloc
                                                                    ]
    let single_space = " "
    let single_colon = ":"
    let concat_array sep (s : string array) =
      let s_len = Array.length s in
      match s_len with
      | 0 -> empty
      | 1 -> Array.unsafe_get s 0
      | _ ->
          let sep_len = String.length sep in
          let len = ref 0 in
          (for i = 0 to s_len - 1 do
             len := ((!len) + (String.length (Array.unsafe_get s i)))
           done;
           (let target = Bytes.create ((!len) + ((s_len - 1) * sep_len)) in
            let hd = Array.unsafe_get s 0 in
            let hd_len = String.length hd in
            String.unsafe_blit hd 0 target 0 hd_len;
            (let current_offset = ref hd_len in
             for i = 1 to s_len - 1 do
               (String.unsafe_blit sep 0 target (!current_offset) sep_len;
                (let cur = Array.unsafe_get s i in
                 let cur_len = String.length cur in
                 let new_off_set = (!current_offset) + sep_len in
                 String.unsafe_blit cur 0 target new_off_set cur_len;
                 current_offset := (new_off_set + cur_len)))
             done;
             Bytes.unsafe_to_string target)))
    let concat3 a b c =
      let a_len = String.length a in
      let b_len = String.length b in
      let c_len = String.length c in
      let len = (a_len + b_len) + c_len in
      let target = Bytes.create len in
      String.unsafe_blit a 0 target 0 a_len;
      String.unsafe_blit b 0 target a_len b_len;
      String.unsafe_blit c 0 target (a_len + b_len) c_len;
      Bytes.unsafe_to_string target
    let concat4 a b c d =
      let a_len = String.length a in
      let b_len = String.length b in
      let c_len = String.length c in
      let d_len = String.length d in
      let len = ((a_len + b_len) + c_len) + d_len in
      let target = Bytes.create len in
      String.unsafe_blit a 0 target 0 a_len;
      String.unsafe_blit b 0 target a_len b_len;
      String.unsafe_blit c 0 target (a_len + b_len) c_len;
      String.unsafe_blit d 0 target ((a_len + b_len) + c_len) d_len;
      Bytes.unsafe_to_string target
    let concat5 a b c d e =
      let a_len = String.length a in
      let b_len = String.length b in
      let c_len = String.length c in
      let d_len = String.length d in
      let e_len = String.length e in
      let len = (((a_len + b_len) + c_len) + d_len) + e_len in
      let target = Bytes.create len in
      String.unsafe_blit a 0 target 0 a_len;
      String.unsafe_blit b 0 target a_len b_len;
      String.unsafe_blit c 0 target (a_len + b_len) c_len;
      String.unsafe_blit d 0 target ((a_len + b_len) + c_len) d_len;
      String.unsafe_blit e 0 target (((a_len + b_len) + c_len) + d_len) e_len;
      Bytes.unsafe_to_string target
    let inter2 a b = concat3 a single_space b
    let inter3 a b c = concat5 a single_space b single_space c
    let inter4 a b c d = concat_array single_space [|a;b;c;d|]
    let parent_dir_lit = ".."
    let current_dir_lit = "."
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
    let lowercase_ascii = String.lowercase_ascii
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
  sig
    val cmp : string -> string -> int
    val find_sorted : string array -> string -> int option
    val find_sorted_assoc : (string * 'a) array -> string -> 'a option
  end =
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
    let find_sorted_assoc (type a) (sorted : (string * a) array)
      (key : string) =
      (let len = Array.length sorted in
       if len = 0
       then None
       else
         (let lo = Array.unsafe_get sorted 0 in
          let c = cmp key (fst lo) in
          if c < 0
          then None
          else
            (let hi = Array.unsafe_get sorted (len - 1) in
             let c2 = cmp key (fst hi) in
             if c2 > 0
             then None
             else binarySearchAssoc sorted 0 (len - 1) key)) : a option)
  end 
module Literals :
  sig
    val js_array_ctor : string
    val js_type_number : string
    val js_type_string : string
    val js_type_object : string
    val js_type_boolean : string
    val js_undefined : string
    val js_prop_length : string
    val param : string
    val partial_arg : string
    val prim : string
    val tmp : string[@@ocaml.doc
                      "temporary varaible used in {!Js_ast_util} "]
    val create : string
    val runtime : string
    val stdlib : string
    val imul : string
    val setter_suffix : string
    val setter_suffix_len : int
    val debugger : string
    val unsafe_downgrade : string
    val fn_run : string
    val method_run : string
    val fn_method : string
    val fn_mk : string
    [@@@ocaml.text " callback actually, not exposed to user yet "]
    val bs_deriving : string
    val bs_deriving_dot : string
    val bs_type : string
    [@@@ocaml.text " nodejs "]
    val node_modules : string
    val node_modules_length : int
    val package_json : string
    val bsconfig_json : string
    val build_ninja : string
    val library_file : string
    val suffix_a : string
    val suffix_cmj : string
    val suffix_cmo : string
    val suffix_cma : string
    val suffix_cmi : string
    val suffix_cmx : string
    val suffix_cmxa : string
    val suffix_ml : string
    val suffix_mlast : string
    val suffix_mlast_simple : string
    val suffix_mliast : string
    val suffix_reast : string
    val suffix_reiast : string
    val suffix_mliast_simple : string
    val suffix_mlmap : string
    val suffix_mll : string
    val suffix_re : string
    val suffix_rei : string
    val suffix_d : string
    val suffix_js : string
    val suffix_bs_js : string
    val suffix_gen_js : string
    val suffix_gen_tsx : string
    val suffix_tsx : string
    val suffix_mli : string
    val suffix_cmt : string
    val suffix_cmti : string
    val commonjs : string
    val es6 : string
    val es6_global : string
    val unused_attribute : string
    val dash_nostdlib : string
    val reactjs_jsx_ppx_2_exe : string
    val reactjs_jsx_ppx_3_exe : string
    val native : string
    val bytecode : string
    val js : string
    val node_sep : string
    val node_parent : string
    val node_current : string
    val gentype_import : string
    val bsbuild_cache : string
    val sourcedirs_meta : string
    val ns_sep_char : char
    val ns_sep : string
  end =
  struct
    let js_array_ctor = "Array"
    let js_type_number = "number"
    let js_type_string = "string"
    let js_type_object = "object"
    let js_type_boolean = "boolean"
    let js_undefined = "undefined"
    let js_prop_length = "length"
    let prim = "prim"
    let param = "param"
    let partial_arg = "partial_arg"
    let tmp = "tmp"
    let create = "create"
    let runtime = "runtime"
    let stdlib = "stdlib"
    let imul = "imul"
    let setter_suffix = "#="
    let setter_suffix_len = String.length setter_suffix
    let debugger = "debugger"
    let unsafe_downgrade = "unsafe_downgrade"
    let fn_run = "fn_run"
    let method_run = "method_run"
    let fn_method = "fn_method"
    let fn_mk = "fn_mk"
    let bs_deriving = "bs.deriving"
    let bs_deriving_dot = "bs.deriving."
    let bs_type = "bs.type"
    let node_modules = "node_modules"[@@ocaml.doc " nodejs "]
    let node_modules_length = String.length "node_modules"
    let package_json = "package.json"
    let bsconfig_json = "bsconfig.json"
    let build_ninja = "build.ninja"
    let library_file = "lib"
    let suffix_a = ".a"
    let suffix_cmj = ".cmj"
    let suffix_cmo = ".cmo"
    let suffix_cma = ".cma"
    let suffix_cmi = ".cmi"
    let suffix_cmx = ".cmx"
    let suffix_cmxa = ".cmxa"
    let suffix_mll = ".mll"
    let suffix_ml = ".ml"
    let suffix_mli = ".mli"
    let suffix_re = ".re"
    let suffix_rei = ".rei"
    let suffix_mlmap = ".mlmap"
    let suffix_cmt = ".cmt"
    let suffix_cmti = ".cmti"
    let suffix_mlast = ".mlast"
    let suffix_mlast_simple = ".mlast_simple"
    let suffix_mliast = ".mliast"
    let suffix_reast = ".reast"
    let suffix_reiast = ".reiast"
    let suffix_mliast_simple = ".mliast_simple"
    let suffix_d = ".d"
    let suffix_js = ".js"
    let suffix_bs_js = ".bs.js"
    let suffix_gen_js = ".gen.js"
    let suffix_gen_tsx = ".gen.tsx"
    let suffix_tsx = ".tsx"
    let commonjs = "commonjs"
    let es6 = "es6"
    let es6_global = "es6-global"
    let unused_attribute = "Unused attribute "
    let dash_nostdlib = "-nostdlib"
    let reactjs_jsx_ppx_2_exe = "reactjs_jsx_ppx_2.exe"
    let reactjs_jsx_ppx_3_exe = "reactjs_jsx_ppx_3.exe"
    let native = "native"
    let bytecode = "bytecode"
    let js = "js"
    let node_sep = "/"[@@ocaml.doc
                        " Used when produce node compatible paths "]
    let node_parent = ".."
    let node_current = "."
    let gentype_import = "genType.import"
    let bsbuild_cache = ".bsbuild"
    let sourcedirs_meta = ".sourcedirs.json"
    let ns_sep_char = '-'
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
    val decode_internal : string -> int ref -> group array
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
    val is_dir_sep : char -> bool
    val maybe_quote : string -> string
    val chop_extension_maybe : string -> string
    val get_extension_maybe : string -> string
    val new_extension : string -> string -> string
    val chop_all_extensions_maybe : string -> string
    val module_name : string -> string
    type module_info = {
      module_name: string ;
      case: bool }
    val as_module : basename:string -> module_info option
  end =
  struct
    let is_dir_sep_unix c = c = '/'
    let is_dir_sep_win_cygwin c = (c = '/') || ((c = '\\') || (c = ':'))
    let is_dir_sep =
      if Sys.unix then is_dir_sep_unix else is_dir_sep_win_cygwin
    let maybe_quote (s : string) =
      let noneed_quote =
        Ext_string.for_all s
          (function
           | '0'..'9'|'a'..'z'|'A'..'Z'|'_'|'+'|'-'|'.'|'/'|'@' -> true
           | _ -> false) in
      if noneed_quote then s else Filename.quote s
    let chop_extension_maybe name =
      let rec search_dot i =
        if (i < 0) || (is_dir_sep (String.unsafe_get name i))
        then name
        else
          if (String.unsafe_get name i) = '.'
          then String.sub name 0 i
          else search_dot (i - 1) in
      search_dot ((String.length name) - 1)
    let get_extension_maybe name =
      let name_len = String.length name in
      let rec search_dot name i name_len =
        if (i < 0) || (is_dir_sep (String.unsafe_get name i))
        then ""
        else
          if (String.unsafe_get name i) = '.'
          then String.sub name i (name_len - i)
          else search_dot name (i - 1) name_len in
      search_dot name (name_len - 1) name_len
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
    let as_module ~basename  =
      let rec search_dot i name name_len =
        if i < 0
        then
          match valid_module_name name name_len with
          | Invalid -> None
          | Upper -> Some { module_name = name; case = true }
          | Lower ->
              Some
                {
                  module_name = (Ext_string.capitalize_ascii name);
                  case = false
                }
        else
          if (String.unsafe_get name i) = '.'
          then
            (match valid_module_name name i with
             | Invalid -> None
             | Upper ->
                 Some
                   {
                     module_name = (Ext_string.capitalize_sub name i);
                     case = true
                   }
             | Lower ->
                 Some
                   {
                     module_name = (Ext_string.capitalize_sub name i);
                     case = false
                   })
          else search_dot (i - 1) name name_len in
      let name_len = String.length basename in
      search_dot (name_len - 1) basename name_len
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
      | Bytecode 
      | Native 
    val deps_of_channel : in_channel -> string list[@@ocaml.doc
                                                     " [deps_of_channel ic]\n    given an input_channel dumps all modules it depend on, only used for debugging \n"]
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
    let deps_of_channel (ic : in_channel) =
      (let size = input_binary_int ic in
       let s = really_input_string ic size in
       let rec aux (s : string) acc (offset : int) size =
         (if offset < size
          then
            let next_tab = String.index_from s offset magic_sep_char in
            aux s ((String.sub s offset (next_tab - offset)) :: acc)
              (next_tab + 1) size
          else acc : string list) in
       aux s [] 1 size : string list)
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
