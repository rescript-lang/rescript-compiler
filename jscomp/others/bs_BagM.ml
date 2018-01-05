

type (+ 'k, 'v ) bag = {
  dict : 'k;
  mutable data : 'v;
} [@@bs.deriving abstract]