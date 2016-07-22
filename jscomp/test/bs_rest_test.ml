

external f : string -> int  [@bs] = "x" [@@bs.call]

let u = f "3" [@bs]
