type a = {x: int}

type b = {dotdotdot: a, y: string}

type c = {dotdotdot: b, z: string}

let v: c = {x: 10, y: "", z: ""}
