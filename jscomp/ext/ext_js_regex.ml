(*It is unfortunate we cannot use a recursive function to process
string and for loop is very limited in OCaml *)
let check_from_end s =
    let rtn = ref false in 
    let () = for i = String.length s - 1 downto 0 do
        let c = String.get s i in 
        if c = '/' && i <> 0 then
            rtn := true
    done in 
    !rtn

(*This does a very simple check*)
(*The first character of a regex string should be a '/'*)
(*The last few characters of a regex string can be a flag, so we go through string
backwards, find the first '/' we encountered. If it is not the first character then 
we have the "/.../..." structure a regex string requires.*)
let js_regex_checker s = 
  let check_first = String.get s 0 = '/' in
  let check_last = check_from_end s in 
  check_first && check_last