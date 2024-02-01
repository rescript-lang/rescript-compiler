@@config({flags: ["-bs-package-output", "es6:jscomp/test:.mjs"]})

@module({from: "./myJson.json", with: {type_: "json", \"some-identifier": "yep"}})
external myJson: Js.Json.t = "default"

Js.log(myJson)
