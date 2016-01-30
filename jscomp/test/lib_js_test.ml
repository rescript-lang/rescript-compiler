


let () =
  begin

    Js.log @@ Js.to_json_string [1;2;3];
    Js.log "hey";
  end

let suites = Mt.[
    "anything_to_string", (fun _ -> Eq("3", Js.anything_to_string 3 ))    
]

;; Mt.from_pair_suites __FILE__ suites
