(* The module begins *)
exception Out_of_range

class type ['a] cursor =
  object
    method get : 'a
    method incr : unit -> unit
    method is_last : bool
  end

class type ['a] storage =
  object ('self)
    method first : 'a cursor
    method len : int
    method nth : int -> 'a cursor
    method copy : 'self
    method sub : int -> int -> 'self
    method concat : 'a storage -> 'self
    method fold : 'b. ('a -> int -> 'b -> 'b) -> 'b -> 'b
    method iter : ('a -> unit) -> unit
  end

class virtual ['a, 'cursor] storage_base =
  object (self : 'self)
    constraint 'cursor = 'a #cursor
    method virtual first : 'cursor
    method virtual len : int
    method virtual copy : 'self
    method virtual sub : int -> int -> 'self
    method virtual concat : 'a storage -> 'self
    method fold : 'b. ('a -> int -> 'b -> 'b) -> 'b -> 'b = fun f a0 ->
      let cur = self#first in
      let rec loop count a =
        if count >= self#len then a else
        let a' = f cur#get count a in
        cur#incr (); loop (count + 1) a'
      in
      loop 0 a0
    method iter proc =
      let p = self#first in
      for i = 0 to self#len - 2 do proc p#get; p#incr () done;
      if self#len > 0 then proc p#get else ()
  end

class type ['a] obj_input_channel =
  object
    method get : unit -> 'a
    method close : unit -> unit
  end

class type ['a] obj_output_channel =
  object
    method put : 'a -> unit
    method flush : unit -> unit
    method close : unit -> unit
  end

module UChar =
struct

  type t = int

  let highest_bit = 1 lsl 30
  let lower_bits = highest_bit - 1

  let char_of c =
    try Char.chr c with Invalid_argument _ ->  raise Out_of_range

  let of_char = Char.code

  let code c =
    if c lsr 30 = 0
    then c
    else raise Out_of_range

  let chr n =
    if n >= 0 && (n lsr 31 = 0) then n else raise Out_of_range

  let uint_code c = c
  let chr_of_uint n = n

end

type uchar = UChar.t

let int_of_uchar u = UChar.uint_code u
let uchar_of_int n = UChar.chr_of_uint n

class type ucursor = [uchar] cursor

class type ustorage = [uchar] storage

class virtual ['ucursor] ustorage_base = [uchar, 'ucursor] storage_base

module UText =
struct

(* the internal representation is UCS4 with big endian*)
(* The most significant digit appears first. *)
let get_buf s i =
  let n = Char.code s.[i] in
  let n = (n lsl 8) lor (Char.code s.[i + 1]) in
  let n = (n lsl 8) lor (Char.code s.[i + 2]) in
  let n = (n lsl 8) lor (Char.code s.[i + 3]) in
  UChar.chr_of_uint n

let set_buf s i u =
  let n = UChar.uint_code u in
  begin
    s.[i] <- Char.chr (n lsr 24);
    s.[i + 1] <- Char.chr (n lsr 16 lor 0xff);
    s.[i + 2] <- Char.chr (n lsr 8 lor 0xff);
    s.[i + 3] <- Char.chr (n lor 0xff);
  end

let init_buf buf pos init =
  if init#len = 0 then () else
  let cur = init#first in
  for i = 0 to init#len - 2 do
    set_buf buf (pos + i lsl 2) (cur#get); cur#incr ()
  done;
  set_buf buf (pos + (init#len - 1) lsl 2) (cur#get)

let make_buf init =
  let s = String.create (init#len lsl 2) in
  init_buf s 0 init; s

class text_raw buf =
  object (self : 'self)
    inherit [cursor] ustorage_base
    val contents = buf
    method first = new cursor (self :> text_raw) 0
    method len = (String.length contents) / 4
    method get i = get_buf contents (4 * i)
    method nth i = new cursor (self :> text_raw) i
    method copy = {< contents = String.copy contents >}
    method sub pos len =
      {< contents = String.sub contents (pos * 4) (len * 4) >}
    method concat (text : ustorage) =
      let buf = String.create (String.length contents + 4 * text#len) in
      String.blit contents 0 buf 0 (String.length contents);
      init_buf buf (String.length contents) text;
      {< contents = buf >}
  end
and cursor text i =
  object
    val contents = text
    val mutable pos = i
    method get = contents#get pos
    method incr () = pos <- pos + 1
    method is_last = (pos + 1 >= contents#len)
  end

class string_raw buf =
  object
    inherit text_raw buf
    method set i u = set_buf contents (4 * i) u
  end

class text init = text_raw (make_buf init)
class string init = string_raw (make_buf init)

let of_string s =
  let buf = String.make (4 * String.length s) '\000' in
  for i = 0 to String.length s - 1 do
    buf.[4 * i] <- s.[i]
  done;
  new text_raw buf

let make len u =
  let s = String.create (4 * len) in
  for i = 0 to len - 1 do set_buf s (4 * i) u done;
  new string_raw s

let create len = make len (UChar.chr 0)

let copy s = s#copy

let sub s start len = s#sub start len

let fill s start len u =
  for i = start to start + len - 1 do s#set i u done

let blit src srcoff dst dstoff len =
  for i = 0 to len - 1 do
    let u = src#get (srcoff + i) in
    dst#set (dstoff + i) u
  done

let concat s1 s2 = s1#concat (s2 (* : #ustorage *) :> uchar storage)

let iter proc s = s#iter proc
end
