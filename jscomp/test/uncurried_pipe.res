module StandardNotation = {
  let add = (. x, y) => x + y
  let addC = (x, y) => x + y

  let v7 = 3->add(. 4)
  let v17 = 10->add(. 3->add(. 4))
  let v27 = 20->add(. 3->addC(4))
  let v37 = 30->addC(3->add(. 4))
}

@@uncurried

open StandardNotation

let v7 = 3->add(4)
let v17 = 10->add(3->add(4))
let v27 = 20->add(3->addC(. 4))
let v37 = 30->addC(. 3->add(4))
