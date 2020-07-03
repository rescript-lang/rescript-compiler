let coord = {x: 3.13, y: 3.14}
let record = {firstField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, secondField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, thirdField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer}

let forceBreak = {
  x: Omega.x,
  y: Theta.y
}

let withSpread = {...initialState, time: nextTime,}
let withSpreadAndForceBreak = {
  ...initialState,
  time: nextTime,
}

let withSpreadAndNaturalBreak = {...fields, firstField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, secondField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer, thirdField: superLongIdentiiiiiiiifffffffiiiiieeeeeeeer}


let x = @attr {x: 1, y: 2}
let x = @attr {...initialState, superLongName: 1, superLongName: 2, superLongName: 5}
let x = @attr {...initialState, superLongName: 1, superLongName: 2, superLongName: 5, superLongName: 20}
