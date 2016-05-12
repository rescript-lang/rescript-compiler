// GENERATED CODE BY BUCKLESCRIPT VERSION 0.3 , PLEASE EDIT WITH CARE
'use strict';


var hostname = "127.0.0.1";

function create_server(http) {
  var server = http.createServer(function (_, resp) {
        resp.statusCode = 200;
        resp.setHeader("Content-Type", "text/plain");
        return resp.end("Hello world\n");
      });
  return server.listen(3000, hostname, function () {
              console.log("Server running at http://" + (hostname + (":" + (3000 + "/"))));
              return /* () */0;
            });
}

exports.create_server = create_server;
/* No side effect */
