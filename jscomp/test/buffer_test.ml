

let v = "gso"



let bytes_equal () = 
  begin
    assert (Bytes.get (Bytes.make 3 'a') 0 = 'a');
    assert (Bytes.unsafe_get (Bytes.make 3 'a') 0 = 'a');
    let u = (Bytes.make 3 'a') in
    (Bytes.unsafe_set u 0 'b');
    assert (Bytes.unsafe_get u 0 = 'b');
    assert (v.[0] = 'g');
  end
let suites = [ 
  "equal",
  bytes_equal;
  "buffer", (fun  _ ->
    let v = Buffer.create 30 in
    for i = 0 to 10 do 
      Buffer.add_string v (string_of_int i)
    done;
    Mt.assert_equal (Buffer.contents v) "012345678910"
            )
]
open Mt 
;; from_suites __FILE__ suites
