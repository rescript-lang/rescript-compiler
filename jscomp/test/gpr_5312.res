@@config({flags : [ "-w", "-103"]})
module type MyModule = {
  type t
  @bs.send.pipe(: t) external someFunction: (string, string) => unit = "someFunction"
}

module MyModule: MyModule = {
  type t
  @bs.send.pipe(: t) external someFunction: (string, string) => unit = "someFunction"
}