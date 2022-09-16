type rec r = {n: int, f: option<r>}

let test1 = (r:r) => r.f.f
