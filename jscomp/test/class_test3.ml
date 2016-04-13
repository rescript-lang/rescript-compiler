class point x_init =
    object
      val mutable x = x_init
      method get_x = x
      method move d = x <- x + d
    end;;

let p = new point 7 


;; Js.log @@ p #get_x  (**  7 *)

class adjusted_point x_init =
    let origin = (x_init / 10) * 10 in
    object
      val mutable x = origin
      method get_x = x
      method get_offset = x - origin
      method move d = x <- x + d
    end

;; Js.log @@ (new adjusted_point 31) # get_x (* 30 *)

class adjusted_point2 x_init =  point ((x_init / 10) * 10)

;; Js.log @@ (new adjusted_point2 31) # get_x (* 30 *)



class printable_point x_init =
    object (s)
      val mutable x = x_init
      method get_x = x
      method move d = x <- x + d
      method print = s#get_x
    end;;

let p = new printable_point 7



;; Js.log  p#print (* 7 *)


let my_int =
  let ints = ref [] in 
    object (self)
      method n = 1
      method register = ints := self :: !ints
      method len = List.length !ints
    end;;


let () = 
  my_int # register ;
  my_int # register ; 
  Js.log @@ my_int # len (* 2 *)

let v = [|0; 3 |]

class printable_point2 x_init =
    let origin = (x_init / 10) * 10 in
    object (self)
      val mutable x = origin
      method get_x = x
      method move d = x <- x + d
      method print = print_int self#get_x
      initializer print_endline ("initializing" ^ __LOC__); v.(0) <- x 
    end;;

let p = new printable_point2 31 

;; Js.log v  (* [30, 3]*)


(* virtual methods *)

class virtual abstract_point x_init =
    object (self)
      method virtual get_x : int
      method get_offset = self#get_x - x_init
      method virtual move : int -> unit
    end;;

class vpoint x_init =
    object
      inherit abstract_point x_init
      val mutable x = x_init
      method get_x = x
      method move d = x <- x + d
    end;;

let v = 
  let h = new vpoint 3  in 
  h # move 32;
  h # get_offset

;;Js.log v (* 32 *)

class virtual abstract_point2 =
    object
      val mutable virtual x : int
      method move d = x <- x + d
    end

class point2 x_init =
    object
      inherit abstract_point2
      val mutable x = x_init
      method get_offset = x - x_init
    end

let vv = 
  let h = new point2 3  in 
  h # move 32;
  h # get_offset

;; Js.log vv (* 32 *) 


class restricted_point x_init =
    object (self)
      val mutable x = x_init
      method get_x = x
      method private move d = x <- x + d
      method bump = self#move 1
    end;;

let p = new restricted_point 0;;

let h = p # bump ; p#get_x 

;; Js.log h  (* 1 *)


class point_again x =
    object (self)
      inherit restricted_point x
      method virtual move : _
    end;;

let hh = 
  let p = new point_again 3 in
  p # move 3 ; 
  p # bump;
  p# bump ; 
  p#get_x 
    
;; Js.log hh  (* 8 *)


class point_again2 x =
    object (self : < move : _; ..> )
      inherit restricted_point x
    end;;

let hhh = 
  let p = new point_again2 3 in
  p # move 30 ; 
  p # bump;
  p# bump ; 
  p#get_x 
    
;; Js.log hhh  (* 35 *)


class point_again3 x =
    object
      inherit restricted_point x as super
      method move = super#move
    end;;


let hhhh = 
  let p = new point_again3 3 in
  p # move 300 ; 
  p # bump;
  p# bump ; 
  p#get_x 

;; Js.log hhhh (* 305 *)
