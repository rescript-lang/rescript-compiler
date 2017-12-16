
type elt = int
type color = Black | Red 
type t =
  | Node of color * t * elt * t 
  | Empty

type enum =
  | End
  | More of elt * t * enum


let blackify = function
  | Node (Red,l, x, r) -> Node(Black,l, x, r), false
  | s -> s, true

let empty = Empty

let is_empty = function
  | Empty -> true
  | _ -> false

let rec mem x = function
  | Empty ->
    false
  | Node(_,l, y, r) -> 
    if x = y then true 
    else if x < y then mem x l
    else  mem x r


let balance_left l x r =
  match l, x, r with
  | Node(Red,Node(Red,a, x, b), y, c), z, d
  | Node(Red,a, x, Node(Red,b, y, c)), z, d ->
    Node(Red, Node(Black,a, x, b), y, Node(Black,c, z, d))
  | l, x, r ->
    Node(Black,l, x, r)

let balance_right l x r =
  match l, x, r with
  | a, x, Node(Red,Node(Red,b, y, c), z, d)
  | a, x, Node(Red,b, y, Node(Red,c, z, d)) ->
    Node(Red, Node(Black,a, x, b), y, Node(Black,c, z, d))
  | l, x, r ->
    Node(Black,l, x, r)



let singleton x =
  Node(Black,Empty, x, Empty)

let unbalanced_left = function
  | Node(Red,Node(Black,a, x, b), y, c) -> balance_left (Node(Red,a, x, b)) y c, false
  | Node(Black,Node(Black,a, x, b), y, c) -> balance_left (Node(Red,a, x, b)) y c, true
  | Node(Black, Node(Red,a, x, Node(Black,b, y, c)), z, d) -> Node(Black,a, x, balance_left (Node(Red,b, y, c)) z d), false
  | _ -> assert false

let unbalanced_right = function
  | Node(Red,a, x, Node(Black,b, y, c)) -> balance_right a x (Node(Red,b, y, c)), false
  | Node(Black,a, x, Node(Black,b, y, c)) -> balance_right a x (Node(Red,b, y, c)), true
  | Node(Black,a, x, Node(Red,Node(Black,b, y, c), z, d)) -> Node(Black,balance_right a x (Node(Red,b, y, c)), z, d), false
  | _ -> assert false

let lbalance x1 x2 x3 = match x1 with
  | Node(Red, Node(Red ,a,x,b), y, c) ->
    Node(Red , Node(Black ,a,x,b), y, Node(Black ,c,x2,x3))
  | Node(Red ,a, x, Node(Red ,b,y,c)) ->
    Node(Red ,Node(Black ,a,x,b), y, Node(Black ,c,x2,x3))
  | _ ->
    Node(Black ,x1,x2,x3)

let lbalance x1 x2 x3 = match x1 with
  | Node(Red, l, y, r) ->    
    begin match l,r with 
      | Node(Red,a,x,b), _ -> 
        Node(Red , Node(Black ,a,x,b), y, Node(Black ,r,x2,x3))
      | _, Node(Red,b,y,c) -> 
        Node(Red ,Node(Black ,l,y,b), y, Node(Black ,c,x2,x3))
      | _ -> Node(Black ,x1,x2,x3)    
    end
  | _ ->
    Node(Black ,x1,x2,x3)    
let rbalance x1 x2 x3 = match x1, x2, x3 with
  | a, x, Node(Red , Node(Red ,b,y,c), z, d) ->
    Node(Red ,Node(Black ,a,x,b), y, Node(Black ,c,z,d))
  | a, x, Node(Red ,b, y, Node(Red ,c,z,d)) ->
    Node(Red , Node(Black ,a,x,b), y, Node(Black ,c,z,d))
  | a,x,b ->
    Node(Black ,a,x,b)  

let rec ins x = function
  | Empty ->
    Node(Red ,Empty, x, Empty)
  | Node(Red ,a, y, b) as s ->
    if x = y then s 
    else if x < y then Node(Red ,ins x a, y, b)
    else  Node(Red ,a, y, ins x b)

  | Node(Black ,a, y, b) as s ->
    if x = y then s 
    else if x < y then lbalance (ins x a) y b
    else rbalance a y (ins x b)

let add x s =

  match ins x s with  
  | Node(Red,a, y, b) -> Node(Black ,a, y, b)
  | s -> s 


let rec remove_min = function
  | Empty
  | Node(Black,Empty, _, Node(Black,_,_,_)) ->
    assert false
  | Node(Black,Empty, x, Empty) ->
    Empty, x, true
  | Node(Black,Empty, x, Node(Red,l, y, r)) ->
    Node(Black,l, y, r), x, false
  | Node(Red,Empty, x, r) ->
    r, x, false
  | Node( c,l, x, r) ->
    let l, y, d = remove_min l in
    let s = Node(c,l, x, r) in
    if d then
      let s, d = unbalanced_right s in s, y, d
    else
      s, y, false


let rec remove_aux x n = 
  match n with 
  | Empty ->
    Empty, false
  | Node( c ,l, y, r) ->
    if x = y then  
      begin match r with
        | Empty ->
          if c = Red then l,false
          else blackify l
        | _ ->
          let r, y, d = remove_min r in
          let n = Node(c,l, y, r) in
          if d then unbalanced_left n else n, false
      end
    else if x < y then
      let l, d = remove_aux x l in
      let n = Node(c,l, y, r) in
      if d then unbalanced_right n else n, false
    else 
      let r, d = remove_aux x r in
      let n = Node(c,l, y, r) in
      if d then unbalanced_left n else n, false        

let remove x s =

  fst (remove_aux x s)

let rec cardinal = function
  | Empty -> 0
  | Node(_,l, x, r)  -> 1 + cardinal l + cardinal r
