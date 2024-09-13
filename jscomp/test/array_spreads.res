let xx = [3]
let x = [1, 2, 3, ...Js.Array2.map(xx, v => v * 2)]
let y = [...x, 4, 5, ...x, 6, ...x->Js.Array2.map(v => v * 2)]
