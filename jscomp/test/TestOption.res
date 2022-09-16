type rec r = {n: int, f: r, fo: option<r>}

let test1 = (r: r) =>
  switch (r.fo.f.f.f.fo, r.fo.f.f.f.fo.n) {
  | (Some({n}), Some(n1)) => n + n1
  | _ => 0
  }
