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

  ("encodeURI", (fun _ -> 
    Eq(encodeURI "[-=-]", "%5B-=-%5D")
  ));

  ("decodeURI", (fun _ -> 
    Eq(decodeURI "%5B-=-%5D", "[-=-]")
  ));

  ("encodeURIComponent", (fun _ -> 
    Eq(encodeURIComponent "[-=-]", "%5B-%3D-%5D")
  ));

  ("decodeURIComponent", (fun _ -> 
    Eq(decodeURIComponent "%5B-%3D-%5D", "[-=-]")
  ));

]

;; Mt.from_pair_suites __FILE__ suites
