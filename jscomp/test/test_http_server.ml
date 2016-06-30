


let port = 3000
let hostname = "127.0.0.1"
let create_server  http = 
  let server = http##createServer begin fun [@uncurry] req  resp  -> 
      resp##statusCode_set 200;
      resp##setHeader "Content-Type" "text/plain";
      resp##end_ "Hello world\n"
    end in
  server##listen port hostname  begin fun [@uncurry] () -> 
    Js.log ("Server running at http://"^ hostname ^ ":" ^ string_of_int port ^ "/")
  end

let () = 
  create_server Http_types.http


