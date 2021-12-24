type t = list(string);

let dot = (s, x) => x @ [s];

let fromString = x => [x];

let toList = x => x;

let toString = x => x |> String.concat("_");

type eq = (t, t);

module NameSet =
  Set.Make({
    type nonrec t = t;
    let rec compare = (x: t, y: t) =>
      switch (x, y) {
      | ([], []) => 0
      | ([], [_, ..._]) => (-1)
      | ([_, ..._], []) => (-1)
      | ([s1, ...rest1], [s2, ...rest2]) =>
        let n = String.compare(s1, s2);
        n != 0 ? n : compare(rest1, rest2);
      };
  });

let rec applyEquation = (~el: t, eq: eq): list(t) =>
  switch (eq, el) {
  | (([], rhs), _) => [rhs @ el]
  | (([s1, ...rest1], rhs), [s2, ...rest2]) =>
    s1 == s2 ? (rest1, rhs) |> applyEquation(~el=rest2) : []
  | (([_, ..._], _), []) => []
  };

let rec applyEquationsToElements =
        (~eqs: list(eq), ~seen, elements: list(t)): list(eq) => {
  let applyEqs = el => {
    let freshElements =
      eqs
      |> List.map(applyEquation(~el))
      |> List.concat
      |> List.filter(y => !NameSet.mem(y, seen));
    freshElements |> List.map(elFresh => (elFresh, el));
  };

  let newEquations = elements |> List.map(applyEqs) |> List.concat;
  let newElements = newEquations |> List.map(fst);
  let newSeen = NameSet.union(seen, newElements |> NameSet.of_list);

  newEquations == []
    ? newEquations
    : newEquations
      @ (newElements |> applyEquationsToElements(~eqs, ~seen=newSeen));
};

/* Apply equations of the form e.g. X.Y = A from the alias: module A = X.Y.
   Return a list of equations on types.
   E.g. if the element is X.Y.t, return equation A.t = X.Y.t */
let applyEquations = (~eqs: list(eq), el: t): list(eq) =>
  [el] |> applyEquationsToElements(~eqs, ~seen=NameSet.empty);