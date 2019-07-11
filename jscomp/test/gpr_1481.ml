type moduleId = < name: string > Js.t

external moduleId : moduleId = "#moduleid" [@@bs.module]

let f () = moduleId##name
