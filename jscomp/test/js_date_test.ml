

let d = Js_date.of_y_m ~month:2. ~year:2016.  ()
let d2 = Js_date.of_y_m_d ~month:2.
let d3 = d2 ~year:2016. ~day:1. ()


let suites = Mt.[
    "getMonth", (fun _ -> Eq(2., d##getMonth () ));
    "getYear", (fun _ -> Eq((2016.,2.,1.), 
                            (d3##getFullYear() , 
                             d3##getMonth(), 
                             d3##getDate()
                            )))
]

;; Mt.from_pair_suites __FILE__ suites
