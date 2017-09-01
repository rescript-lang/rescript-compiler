



let x =  [%obj{x = 3 ; y = 4}]##x

;; [%obj{x = 3 ; y = 4}]##x


let zz =  [%obj{_5 = 3 }]##_5


;; [%obj{_5 = 3 }]##_5

;; Js.log @@ Obj.tag (Obj.repr [%obj{_5 = 3 }])