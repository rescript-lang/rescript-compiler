


let () = Js.log "你好"

let () = Js.log {j|你好|j}  

let () = 
    Js.log [%raw {|"你好"|}]
