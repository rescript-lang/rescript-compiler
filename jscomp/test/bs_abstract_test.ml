

type 'a linked_list = 
  {
    hd : 'a ; 
    mutable tl : 'a linked_list Js.null
  }
  [@@bs.deriving abstract]



let v = linked_list ~hd:3 ~tl:Js.null   

;; tlSet v (Js.Null.return v)