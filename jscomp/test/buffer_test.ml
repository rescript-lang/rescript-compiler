

let v = "gso"




let suites = Mt.[ 
  "equal",  (fun _ -> 
    begin
      Eq ((Bytes.get (Bytes.make 3 'a') 0 , Bytes.unsafe_get (Bytes.make 3 'a') 0),
          ('a', 'a'));

    end)
    ; "equal2", (fun _ ->       let u = (Bytes.make 3 'a') in
    (Bytes.unsafe_set u 0 'b');
    Eq ((Bytes.unsafe_get u 0 ,v.[0]) ,  ('b', 'g'));
                );
  "buffer", (fun  _ ->
    let v = Buffer.create 30 in
    for i = 0 to 10 do 
      Buffer.add_string v (string_of_int i)
    done;
    Eq (Buffer.contents v, "012345678910")
            )
]

;; Mt.from_pair_suites __MODULE__ suites
