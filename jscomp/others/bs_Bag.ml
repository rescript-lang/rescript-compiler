

type (+ 'k, + 'v) bag = {
  dict : 'k ;
  data : 'v
} [@@bs.deriving abstract]