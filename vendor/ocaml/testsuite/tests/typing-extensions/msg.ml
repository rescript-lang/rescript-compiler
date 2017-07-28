(* Typed names *)

module Msg : sig

  type 'a tag

  type result = Result : 'a tag * 'a -> result

  val write : 'a tag -> 'a -> unit

  val read : unit -> result

  type 'a tag += Int : int tag

  module type Desc = sig
    type t
    val label : string
    val write : t -> string
    val read : string -> t
  end

  module Define (D : Desc) : sig
    type 'a tag += C : D.t tag
  end

end = struct

  type 'a tag = ..

  type ktag = T : 'a tag -> ktag

  type 'a kind =
  { tag : 'a tag;
    label : string;
    write : 'a -> string;
    read : string -> 'a; }

  type rkind = K : 'a kind -> rkind

  type wkind = { f : 'a . 'a tag -> 'a kind }

  let readTbl : (string, rkind) Hashtbl.t = Hashtbl.create 13

  let writeTbl : (ktag, wkind) Hashtbl.t = Hashtbl.create 13

  let read_raw () : string * string = raise (Failure "Not implemented")

  type result = Result : 'a tag * 'a -> result

  let read () =
    let label, content = read_raw () in
      let K k = Hashtbl.find readTbl label in
        let body = k.read content in
          Result(k.tag, body)

  let write_raw (label : string) (content : string) =
    raise (Failure "Not implemented")

  let write (tag : 'a tag) (body : 'a) =
    let {f} = Hashtbl.find writeTbl (T tag) in
    let k = f tag in
    let content = k.write body in
      write_raw k.label content

  (* Add int kind *)

  type 'a tag += Int : int tag

  let ik =
    { tag = Int;
      label = "int";
      write = string_of_int;
      read = int_of_string }

  let () = Hashtbl.add readTbl "int" (K ik)

  let () =
    let f (type t) (i : t tag) : t kind =
      match i with
        Int -> ik
      | _ -> assert false
    in
      Hashtbl.add writeTbl (T Int) {f}

  (* Support user defined kinds *)

  module type Desc = sig
    type t
    val label : string
    val write : t -> string
    val read : string -> t
  end

  module Define (D : Desc) = struct
    type 'a tag += C : D.t tag
    let k =
      { tag = C;
        label = D.label;
        write = D.write;
        read = D.read }
    let () = Hashtbl.add readTbl D.label (K k)
    let () =
      let f (type t) (c : t tag) : t kind =
        match c with
          C -> k
        | _ -> assert false
      in
        Hashtbl.add writeTbl (T C) {f}
  end

end;;

let write_int i = Msg.write Msg.Int i;;

module StrM = Msg.Define(struct
  type t = string
  let label = "string"
  let read s = s
  let write s = s
end);;

type 'a Msg.tag += String = StrM.C;;

let write_string s = Msg.write String s;;

let read_one () =
  let Msg.Result(tag, body) = Msg.read () in
  match tag with
    Msg.Int -> print_int body
  | String -> print_string body
  | _ -> print_string "Unknown";;
