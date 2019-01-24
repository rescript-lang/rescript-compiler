type _ t = I : int t;;

let f (type a) (x : a t) =
  let module M = struct
    let (I : a t) = x     (* fail because of toplevel let *)
    let x = (I : a t)
  end in
  () ;;
[%%expect{|
type _ t = I : int t
Line _, characters 9-10:
Error: This pattern matches values of type int t
       but a pattern was expected which matches values of type a t
       Type int is not compatible with type a
|}];;

(* extra example by Stephen Dolan, using recursive modules *)
(* Should not be allowed! *)
type (_,_) eq = Refl : ('a, 'a) eq;;

let bad (type a) =
 let module N = struct
   module rec M : sig
     val e : (int, a) eq
   end = struct
     let (Refl : (int, a) eq) = M.e  (* must fail for soundness *)
     let e : (int, a) eq = Refl
   end
 end in N.M.e
;;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
Line _, characters 10-14:
Error: This pattern matches values of type (int, int) eq
       but a pattern was expected which matches values of type (int, a) eq
       Type int is not compatible with type a
|}];;
