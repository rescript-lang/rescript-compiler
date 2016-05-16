


let port = 3000
let hostname = "127.0.0.1"
let create_server  http = 
  let server = http##createServer (fun %uncurry  (req,  resp)  -> 
      resp##statusCode__set 200;
      resp##setHeader("Content-Type", "text/plain");
      resp##end__("Hello world\n")
    )
  in
  server##listen(port, hostname,  fun %uncurry () -> 
      Js.log ("Server running at http://"^ hostname ^ ":" ^ string_of_int port ^ "/")
    ) 
let () = 
  create_server Http_types.http


