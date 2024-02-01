@@config({flags: ["-bs-package-output", "es6:jscomp/test:.mjs"]})

@module({from: "./myJson.json", type_: "json"})
external myJson: Js.Json.t = "default"

Js.log(myJson)
