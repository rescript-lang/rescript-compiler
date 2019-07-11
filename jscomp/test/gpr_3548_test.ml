type orientation =
  [`Horizontal[@bs.as "horizontal"] | `Vertical[@bs.as "vertical"]]
[@@bs.deriving jsConverter]

let () = Js.log (orientationToJs `Horizontal)
