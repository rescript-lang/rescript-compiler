
class point x_init = object
  val mutable x = x_init
  method get_x = x
  method move d = x <- x + d
end;;

let p = new point 7;;

p#get_x;;
p#move 3;;
p#get_x;;

let q = Oo.copy p;;

q#move 7; p#get_x, q#get_x;;

class color_point x (c : string) = object
  inherit point x
  val c = c
  method color = c
end;;

let p' = new color_point 5 "red";;

p'#get_x, p'#color;;

let l = [p; (p' :> point)];;

let get_x p = p#get_x;;
let set_x p = p#set_x;;
List.map get_x l;;

class ref x_init = object
  val mutable x = x_init
  method get = x
  method set y = x <- y
end;;

class ref (x_init:int) = object
  val mutable x = x_init
  method get = x
  method set y = x <- y
end;;

class ['a] ref x_init = object
  val mutable x = (x_init : 'a)
  method get = x
  method set y = x <- y
end;;

let r = new ref 1 in r#set 2; (r#get);;

class ['a] circle (c : 'a) = object
  val mutable center = c
  method center = center
  method set_center c = center <- c
  method move = (center#move : int -> unit)
end;;

class ['a] circle (c : 'a) = object
  constraint 'a = #point
  val mutable center = c
  method center = center
  method set_center c = center <- c
  method move = center#move
end;;

let (c, c') = (new circle p, new circle p');;

class ['a] color_circle c = object
  constraint 'a = #color_point
  inherit ['a] circle c
  method color = center#color
end;;

let c'' = new color_circle p;;
let c'' = new color_circle p';;

(c'' :> color_point circle);;
(c'' :> point circle);;                 (* Echec *)
fun x -> (x : color_point color_circle :> point circle);;

class printable_point y = object (s)
  inherit point y
  method print = print_int s#get_x
end;;

let p = new printable_point 7;;
p#print;;

class printable_color_point y c = object (self)
  inherit color_point y c
  inherit printable_point y as super
  method print =
    print_string "(";
    super#print;
    print_string ", ";
    print_string (self#color);
    print_string ")"
end;;

let p' = new printable_color_point 7 "red";;
p'#print;;

class functional_point y = object
  val x = y
  method get_x = x
  method move d = {< x = x + d >}
end;;

let p = new functional_point 7;;

p#get_x;;
(p#move 3)#get_x;;
p#get_x;;

fun x -> (x :> functional_point);;

(*******************************************************************)

class virtual ['a] lst () = object (self)
  method virtual null : bool
  method virtual hd : 'a
  method virtual tl : 'a lst
  method map f =
    (if self#null then
       new nil ()
     else
       new cons (f self#hd) (self#tl#map f)
     : 'a lst)
  method iter (f : 'a -> unit) =
    if self#null then ()
    else begin
      f self#hd;
      self#tl#iter f
    end
  method print (f : 'a -> unit) =
    print_string "(";
    self#iter (fun x -> f x; print_string "::");
    print_string "[]";
    print_string ")"
end and ['a] nil () = object
  inherit ['a] lst ()
  method null = true
  method hd   = failwith "hd"
  method tl   = failwith "tl"
end and ['a] cons h t = object
  inherit ['a] lst ()
  val h = h val t = t
  method null = false
  method hd   = h
  method tl   = t
end;;

let l1 = new cons 3 (new cons 10 (new nil ()));;

l1#print print_int;;

let l2 = l1#map (fun x -> x + 1);;
l2#print print_int;;

let rec map_list f (x:'a lst) =
  if x#null then new nil()
  else new cons (f x#hd) (map_list f x#tl);;

let p1 = (map_list (fun x -> new printable_color_point x "red") l1);;
p1#print (fun x -> x#print);;

(*******************************************************************)

class virtual comparable () = object (self : 'a)
  method virtual cmp : 'a -> int
  end;;

class int_comparable (x : int) = object
  inherit comparable ()
  val x = x
  method x = x
  method cmp p = compare x p#x
end;;

class int_comparable2 xi = object
  inherit int_comparable xi
  val mutable x' = xi
  method set_x y = x' <- y
end;;

class ['a] sorted_list () = object
  constraint 'a = #comparable
  val mutable l = ([] : 'a list)
  method add x =
    let rec insert =
      function
            []     ->  [x]
      | a::l as l' -> if a#cmp x <= 0 then a::(insert l) else x::l'
    in
      l <- insert l
  method hd = List.hd l
end;;

let l = new sorted_list ();;
let c = new int_comparable 10;;
l#add c;;

let c2 = new int_comparable2 15;;
l#add (c2 :> int_comparable);;      (* Echec : 'a comp2 n'est un sous-type *)
(new sorted_list ())#add c2;;

class int_comparable3 (x : int) = object
  val mutable x = x
  method cmp (y : int_comparable) = compare x y#x
  method x = x
  method setx y = x <- y
end;;

let c3 = new int_comparable3 15;;
l#add (c3 :> int_comparable);;
(new sorted_list ())#add c3;;   (* Error; strange message with -principal *)

let sort (l : #comparable list) = List.sort (fun x -> x#cmp) l;;
let pr l =
  List.map (fun c -> print_int c#x; print_string " ") l;
  print_newline ();;
let l = [new int_comparable 5; (new int_comparable3 2 :> int_comparable);
         new int_comparable 4];;
pr l;;
pr (sort l);;
let l = [new int_comparable2 2; new int_comparable2 0];;
pr l;;
pr (sort l);;

let min (x : #comparable) y =
  if x#cmp y <= 0 then x else y;;

(min (new int_comparable  7) (new int_comparable 11))#x;;
(min (new int_comparable2 5) (new int_comparable2 3))#x;;

(*******************************************************************)

class ['a] link (x : 'a) = object (self : 'b)
  val mutable x = x
  val mutable next = (None : 'b option)
  method x = x
  method  next = next
  method  set_x y = x <- y
  method  set_next l = next <- l
  method  append l =
    match next with
      None ->
        self#set_next l
    | Some l' ->
        l'#append l
end;;

class ['a] double_link x = object (self)
  inherit ['a] link x
  val mutable prev = None
  method prev = prev
  method  set_next l =
         next <- l;
         match l with Some l -> l#set_prev (Some self) | None -> ()
  method  set_prev l = prev <- l
end;;

let rec fold_right f (l : 'a #link option) accu =
  match l with
    None -> accu
  | Some l ->
      f l#x (fold_right f l#next accu);;

(*******************************************************************)

class calculator () = object (self)
  val mutable arg = 0.
  val mutable acc = 0.
  val mutable equals = function s -> s#arg
  method arg = arg
  method acc = acc
  method enter n = arg <- n; self
  method add =
    acc <- equals self;
    equals <- (function s -> s#acc +. s#arg);
    self
  method sub =
    acc <- equals self;
    equals <- (function s -> s#acc -. s#arg);
    self
  method equals = equals self
end;;

((new calculator ())#enter 5.)#equals;;
(((new calculator ())#enter 5.)#sub#enter 3.5)#equals;;
((new calculator ())#enter 5.)#add#add#equals;;

class calculator () = object (self)
  val mutable arg = 0.
  val mutable acc = 0.
  val mutable equals = function s -> s#arg
  method arg = arg
  method acc = acc
  method enter n = arg <- n; self
  method add = {< acc = equals self; equals = function s -> s#acc +. s#arg >}
  method sub = {< acc = equals self; equals = function s -> s#acc -. s#arg >}
  method equals = equals self
end;;

((new calculator ())#enter 5.)#equals;;
(((new calculator ())#enter 5.)#sub#enter 3.5)#equals;;
((new calculator ())#enter 5.)#add#add#equals;;

class calculator arg acc = object (self)
  val arg = arg
  val acc = acc
  method enter n = new calculator n acc
  method add = new calculator_add arg self#equals
  method sub = new calculator_sub arg self#equals
  method equals = arg
end and calculator_add arg acc = object
  inherit calculator arg acc
  method enter n = new calculator_add n acc
  method equals = acc +. arg
end and calculator_sub arg acc = object
  inherit calculator arg acc
  method enter n = new calculator_sub n acc
  method equals = acc -. arg
end;;

let calculator = new calculator 0. 0.;;

(calculator#enter 5.)#equals;;
((calculator#enter 5.)#sub#enter 3.5)#equals;;
(calculator#enter 5.)#add#add#equals;;
