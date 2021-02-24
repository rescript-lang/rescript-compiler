

type moduleId = < name : string > 
  
external moduleId : moduleId = "#moduleid" [@@bs.module]


let f () =
  moduleId##name
