'use strict';

var Http = require("http");

var hostname = "127.0.0.1";

function create_server(http) {
  var server = http.createServer((function (_, resp) {
          resp.statusCode = 200;
          resp.setHeader("Content-Type", "text/plain");
          return resp.end("Hello world\n");
        }));
  return server.listen(3000, hostname, (function () {
                console.log("Server running at http://" + (hostname + (":" + (String(3000) + "/"))));
                return /* () */0;
              }));
}

create_server(Http);

/*  Not a pure module */
