@@config({
  flags: [
    /* "-w";
     "@A" */
    /* "-drawlambda"; */
    /* "-dtypedtree"; */
    /* "-bs-diagnose"; */
    "-dparsetree",
    /* "-dsource"; */
  ],
})
type t = {x: int, y: int}

// let f = (x,y) => {
//     let {} = {x,y}
//     x + y
// }

let f = (window, a, b) => {
  window["location"](. a, b)
}
