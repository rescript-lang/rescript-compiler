exception UnsafeFindFailed(string);

let isEmpty = l =>
  switch (l) {
  | [_h, ..._t] => false
  | [] => true
  };

let isNotEmpty = l => !(l |> isEmpty);

let findOpt = (p, l) =>
  try (Some(List.find(p, l))) {
  | Not_found => None
  };

let unsafeFind = (p, message, l) =>
  try (List.find(p, l)) {
  | Not_found =>
    Rollbar.error(message);
    raise(UnsafeFindFailed(message));
  };

let distinct = l => {
  let rec aux = (l, d) =>
    switch (l) {
    | [head, ...tail] =>
      if (d |> List.exists(u => u == head)) {
        aux(tail, d);
      } else {
        aux(tail, [head, ...d]);
      }
    | [] => d
    };

  aux(l, []);
};

let swapDown = (e, l) => {
  let rec aux = (prev, l, e) =>
    switch (l) {
    | [head, next, ...tail] when head == e => prev @ [next, head, ...tail]
    | [head, ...tail] => aux(prev @ [head], tail, e)
    | [] => prev
    };

  aux([], l, e);
};

let swapUp = (e, l) => l |> List.rev |> swapDown(e) |> List.rev;

let swap = (up, e, l) => up ? l |> swapUp(e) : l |> swapDown(e);
