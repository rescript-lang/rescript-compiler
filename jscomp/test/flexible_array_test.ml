type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec sub (tr : _ tree) k =
  match tr with
  | Lf -> raise Not_found
  | Br (v, l, r) ->
      if k = 1 then v else if k mod 2 = 0 then sub l (k / 2) else sub r (k / 2)

let rec update (tr : _ tree) k w =
  match tr with
  | Lf -> if k = 1 then Br (w, Lf, Lf) else raise Not_found
  | Br (v, l, r) ->
      if k = 1 then Br (w, l, r)
      else if k mod 2 = 0 then Br (v, update l (k / 2) w, r)
      else Br (v, l, update r (k / 2) w)

let rec delete (tr : _ tree) n =
  match tr with
  | Lf -> raise Not_found
  | Br (v, l, r) ->
      if n = 1 then Lf
      else if n mod 2 = 0 then Br (v, delete l (n / 2), r)
      else Br (v, l, delete r (n / 2))

(* it is the same as [push_front] *)

(** proof: left => right right has to be left *)
let rec loext (tr : _ tree) w =
  match tr with
  | Lf -> Br (w, Lf, Lf)
  | Br (v, l, r) ->
      Br
        ( w
        , loext r v
          (* v at position 2 contribute to 4, 6, 8,12, 10, 14 from 3 5 ,... *)
        , l )

(* pop_back *)
let rec lorem (tr : _ tree) =
  match tr with
  | Lf -> raise Not_found
  | Br (w, Lf, Lf) -> Lf
  (* length = 1 *)
  | Br (w, (Br (v, ll, lr) as l), r) ->
      (* length >= 2 *)
      Br (v, r, lorem l)
  | _ -> assert false

module Int_array : sig
  type t

  val empty : t
  val get : t -> int -> int
  val set : t -> int -> int -> t
  val push_front : t -> int -> t
  val pop_front : t -> t
  val push_back : t -> int -> t
  val pop_back : t -> t
  val pp : Format.formatter -> t -> unit
  val append : t -> t -> t
  val sort : t -> t
  val of_array : int array -> t
  val equal : t -> t -> bool
end = struct
  type t = int tree * int

  let empty = (Lf, 0)
  let length (_, n) = n

  let get (tree, k) i =
    if i >= 0 && i < k then sub tree (i + 1) else invalid_arg "Array.get"

  let set (tree, k) i v =
    if i >= 0 && i < k then (update tree (i + 1) v, k)
    else invalid_arg "Array.set"

  let push_front (tree, k) v = (loext tree v, k + 1)

  let pop_front (tree, k) =
    if k > 0 then (lorem tree, k - 1) else invalid_arg "Array.pop_front"

  let push_back (tree, k) v = (update tree (k + 1) v, k + 1)

  let pop_back (tree, k) =
    if k > 0 then (delete tree k, k - 1) else invalid_arg "Array.pop_back"

  let pp fmt s =
    let v = ref "[ " in
    for i = 0 to length s - 1 do
      v := !v ^ ", " ^ string_of_int (get s i)
    done ;
    v := !v ^ "]" ;
    Format.fprintf fmt "%s" !v

  let filter_from i p s =
    let u = ref empty in
    for i = i to length s - 1 do
      let ele = get s i in
      if p ele then u := push_back !u ele
    done ;
    !u

  let append a b =
    (* let size = size a + size b in *)
    let empty = ref empty in
    for i = 0 to length a - 1 do
      empty := push_back !empty (get a i)
    done ;
    for i = 0 to length b - 1 do
      empty := push_back !empty (get b i)
    done ;
    !empty

  let rec sort s =
    let size = length s in
    if size <= 1 then s
    else
      let head = get s 0 in
      let larger = sort @@ filter_from 1 (fun x -> x > head) s in
      let smaller = sort @@ filter_from 1 (fun x -> x <= head) s in
      append smaller (push_front larger head)

  let of_array arr =
    let v = ref empty in
    for i = 0 to Array.length arr - 1 do
      v := push_back !v arr.(i)
    done ;
    !v

  let equal (x : t) (y : t) = x = y
end

let ( =~ ) x y = Int_array.equal x (Int_array.of_array y)

let _ =
  let u = Int_array.of_array [|1; 2; 2; 5; 3; 6|] in
  assert (Int_array.sort u =~ [|1; 2; 2; 3; 5; 6|]) ;
  let len = 500 in
  let v = Array.init len (fun i -> len - i) in
  Int_array.sort (Int_array.of_array v) =~ Array.init len (fun i -> i + 1)
