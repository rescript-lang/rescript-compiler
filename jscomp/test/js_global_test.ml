open Js_global

let suites = Mt.[
  ("setTimeout/clearTimeout sanity check", (fun _ -> 
    let handle = setTimeout (fun () -> ()) 0 in
    clearTimeout handle;
    Ok true
  ));
  ("setInerval/clearInterval sanity check", (fun _ -> 
    let handle = setInterval (fun () -> ()) 0 in
    clearInterval handle;
    Ok true
  ));
]

;; Mt.from_pair_suites __FILE__ suites
