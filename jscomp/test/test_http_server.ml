let port = 3000
let hostname = "127.0.0.1"

let create_server http =
  let server =
    http##createServer (fun [@bs] req resp ->
        resp ## statusCode #= 200 ;
        resp##setHeader "Content-Type" "text/plain" ;
        resp##_end "Hello world\n") in
  server##listen port hostname (fun [@bs] () ->
      Js.log
        ( "Server running at http://" ^ hostname ^ ":" ^ string_of_int port
        ^ "/" ))

let () = create_server Http_types.http
