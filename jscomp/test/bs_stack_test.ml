

type node = {
  value : int ; 
  left : t;
  right : t
} 
and t = node Js.null
[@@bs.deriving abstract]



module S = Bs.Stack
module Q = Bs.Queue 

let inOrder (v : t) = 
  let current =  ref v in 
  let s : node S.t = S.create () in 
  let q : int Q.t = Q.create () in 
  while !current != Js.null do 
    let v = Js.Null.getUnsafe !current  in 
    S.push s v; 
    current := left v; 
  done ;
  while not (S.isEmpty s ) do 
    current := S.popNull s ;
    let v = Js.Null.getUnsafe !current in 
    Q.push q (value v);
    current := right v ;
    while !current != Js.null do 
      let v = Js.Null.getUnsafe !current  in 
      S.push s v; 
      current := left v; 
    done ;    
  done; 
  Q.toArray q 

let inOrder3 (v : t) = 
  let current =  ref v in 
  let s : node S.t = S.create () in 
  let q : int Q.t = Q.create () in 
  while !current != Js.null do 
    let v = Js.Null.getUnsafe !current  in 
    S.push s v; 
    current := left v; 
  done ;
  S.dynamicPopIter s begin fun [@bs] popped -> 
    Q.push q (value popped);
    let current = ref (right popped) in 
    while !current != Js.null do 
      let v = Js.Null.getUnsafe !current in 
      S.push s v;  
      current := left v
    done 
  end;
  Q.toArray q 

let inOrder2 (v : t) =   
  let todo = ref true in 
  let cursor = ref v in 
  let s : node S.t = S.create () in 
  let q : int Q.t = Q.create () in 
  while !todo do 
    if !cursor != Js.null then 
      (
        let v = (Js.Null.getUnsafe !cursor) in 
        S.push s v;
        cursor := left v)
    else 
      begin 
        if not (S.isEmpty s) then 
          (cursor := S.popNull s ;
           let current = Js.Null.getUnsafe !cursor in 
           Q.push q (value current);
           cursor := right current)
        else 
          todo := false
      end 
  done 

let n
    ?l ?r a =
  node   ~value:a 
    ~left:(Js.Null.fromOption l) 
    ~right:(Js.Null.fromOption r)


let test1 = 
  n 1
    ~l:
      (n 2 
         ~l:(n 4 )
         ~r:(n 5))
    ~r:(n 3)

let pushAllLeft st1 s1 = 
  let current = ref st1 in 
  while !current != Js.null do 
    let v = Js.Null.getUnsafe !current  in 
    S.push s1 v; 
    current := left v; 
  done 
;;


let test2 = 
  n 3
  ~l:(
    n 1 
    ~l:(
      n 5
      ~l:
      (n 2
      ~l:
      (n 4)
      )
    )
  )

let test3 = 
  
    n 1 
    ~l:(
      n 5
      ~l:
      (n 2
      ~l:
      (n 4)
      )
    )
  ~r:(n 3)

let () =     
  Js.log (inOrder (Js.Null.return test1));
  Js.log (inOrder3 (Js.Null.return test1))
