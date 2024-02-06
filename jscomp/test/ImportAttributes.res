@@config({flags: ["-bs-package-output", "es6:jscomp/test:.mjs"]})

@module({from: "./myJson.json", with: {type_: "json", \"some-identifier": "yep"}})
external myJson: Js.Json.t = "default"

Js.log(myJson)

@module({from: "./myCss.css", with: {type_: "css", \"some-identifier": "yep"}})
external buttonCss: string = "button"

Js.log(buttonCss)
