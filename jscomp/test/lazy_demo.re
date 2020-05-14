let lazy1 = lazy {
    "Hello, lazy" -> Js.log;
     1
};

let lazy2 = lazy 3 ;

Js.log2 (lazy1, lazy2);

let (lazy la, lazy lb) = (lazy1, lazy2);

Js.log2 (la, lb);