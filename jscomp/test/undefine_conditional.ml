

#ifndef RELEASE
let x = 3
#else 
let x = 4   
#endif 

#ifndef OCAML_VERSION
let y = 5

#else 
let y = 6

#endif 

#ifdef RELEASE

let h = 0
#endif

#ifdef OCAML_VERSION
let has_ocaml_version = true
#endif  


#if (defined BROWSER || defined RELEASE)
let hh = 1
#endif  