type rec r = {n: int, f: option<r>}

let test1 = (r:r) => r.f.f

type rec r2 = {n: int, f2?: r2}

let getF2 = r2 => r2.f2

let test2 = (r2:r2) => r2->getF2->getF2
