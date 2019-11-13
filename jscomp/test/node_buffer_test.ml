
let suites = Mt.[
  "alloc returns an object", (fun _ -> 
    let buf = Node.Buffer.alloc 0 () in
    Eq(Js.typeof buf, "object")
  );
  "alloc returns buffer of specified length", (fun _ -> 
    let buf = Node.Buffer.alloc 3 () in
    Eq(Node.Buffer.length buf, 3)
  );
  "alloc takes optional fill parameter to fill buffer (String)", (fun _ -> 
    let buf = Node.Buffer.alloc 3 ~fill:(`String "a") () in
    Eq(Node.Buffer.toString buf, "aaa")
  );
  "alloc takes optional fill parameter to fill buffer (Integer)", (fun _ -> 
    let buf = Node.Buffer.alloc 3 ~fill:(`Integer 98) () in
    Eq(Node.Buffer.toString buf, "bbb")
  );
  "alloc takes optional fill parameter to fill buffer (Buffer)", (fun _ -> 
    let fill_buffer = Node.Buffer.alloc 3 ~fill:(`String "abc") () in
    let buf = Node.Buffer.alloc 6 ~fill:(`Buffer fill_buffer) () in
    Eq(Node.Buffer.toString buf, "abcabc")
  );
  "alloc takes optional fill parameter to fill buffer (Uint8Array)", (fun _ -> 
    let fill_arr = Js_typed_array2.Uint8Array.make [|97;98;99|] in
    let buf = Node.Buffer.alloc 9 ~fill:(`Uint8Array fill_arr) () in
    Eq(Node.Buffer.toString buf, "abcabcabc")
  );
  "alloc takes optional encoding parameter to spec fill encoding", (fun _ -> 
    let str = "YWJj" in 
    let buf = Node.Buffer.alloc 3 ~fill:(`String str) ~encoding:`base64 () in
    Eq(Node.Buffer.toString buf, "abc")
  );

  "allocUnsafe returns an object", (fun _ -> 
    let buf = Node.Buffer.allocUnsafe 4 in
    Eq(Js.typeof buf, "object")
  );
  "allocUnsafe returns buffer of specified length", (fun _ -> 
    let buf = Node.Buffer.allocUnsafe 4 in
    Eq(Node.Buffer.length buf, 4)
  );

  "allocUnsafeSlow returns an object", (fun _ -> 
    let buf = Node.Buffer.allocUnsafeSlow 4 in
    Eq(Js.typeof buf, "object")
  );
  "allocUnsafeSlow returns buffer of specified length", (fun _ -> 
    let buf = Node.Buffer.allocUnsafeSlow 4 in
    Eq(Node.Buffer.length buf, 4)
  );

  "byteLength returns byte length of value - Buffer", (fun _ ->
    let buf = Node.Buffer.alloc 4 () in
    Eq(Node.Buffer.byteLength (`Buffer buf), 4)
  );
  "byteLength returns byte length of value - Int32Array", (fun _ ->
    let arr = Js.TypedArray2.Int32Array.fromLength 3 in
    Eq(Node.Buffer.byteLength (`Int32Array arr), 3 * 4)
  );

  "byteLengthOfString returns byte length of string", (fun _ -> 
    Eq(Node.Buffer.byteLengthOfString "abc" (), 3)
  );
  "byteLengthOfString returns byte length of string", (fun _ -> 
    Eq(Node.Buffer.byteLengthOfString "YWJj" ~encoding:`base64 (), 3)
  );

  "compare returns -1 if first param sorts before second", (fun _ -> 
    let buf1 = Node.Buffer.fromString "a"
    and buf2 = Node.Buffer.fromString "b" in
    Eq(Node.Buffer.compare (`Buffer buf1) (`Buffer buf2), -1)
  );
  "compare returns 0 if first param equals to second", (fun _ -> 
    let buf1 = Node.Buffer.fromString "a"
    and buf2 = Node.Buffer.fromString "a" in
    Eq(Node.Buffer.compare (`Buffer buf1) (`Buffer buf2), 0)
  );
  "compare returns 1 if first param sorts after second", (fun _ -> 
    let buf1 = Node.Buffer.fromString "b"
    and buf2 = Node.Buffer.fromString "a" in
    Eq(Node.Buffer.compare (`Buffer buf1) (`Buffer buf2), 1)
  );

  "compareRanges compares values from two buffers", (fun _ ->
    let buf1 = Node.Buffer.fromString "bbbabcbbb"
    and buf2 = Node.Buffer.fromString "babcbbb" in
    Eq(Node.Buffer.compareRanges buf1 buf2 ~targetStart:1 ~targetEnd:3 ~sourceStart:3 ~sourceEnd:5 (), 0)
  );

  "concat returns contactenated buffer from array of buffers", (fun _ -> 
    let buf1 = Node.Buffer.fromString "a"
    and buf2 = Node.Buffer.fromString "b" in
    let concatenated = Node.Buffer.concat [|buf1; buf2|] in
    Eq(Node.Buffer.toString concatenated, "ab")
  );

  "concatWithLength returns contactenated buffer from array of buffers", (fun _ -> 
    let buf1 = Node.Buffer.fromString "a"
    and buf2 = Node.Buffer.fromString "bc" in
    let concatenated = Node.Buffer.concatWithLength [|buf1; buf2|] 3 in
    Eq(Node.Buffer.toString concatenated, "abc")
  );

  "fromArray", (fun _ -> 
    let buf = Node.Buffer.fromArray [|97;98|] in
    Eq(Node.Buffer.toString buf, "ab")
  );

  "fromArrayBuffer", (fun _ -> 
    let buf = Node.Buffer.fromArrayBuffer (Js.TypedArray2.ArrayBuffer.make 4) () in
    Eq(Node.Buffer.length buf, 4)
  );
  "fromArrayBuffer supports optional byteOffset param", (fun _ -> 
    let buf = Node.Buffer.fromArrayBuffer (Js.TypedArray2.ArrayBuffer.make 4) ~byteOffset:2 () in
    Eq(Node.Buffer.length buf, 2)
  );
  "fromArrayBuffer supports optional byteOffset and length param ", (fun _ -> 
    let buf = Node.Buffer.fromArrayBuffer (Js.TypedArray2.ArrayBuffer.make 4) ~byteOffset:2 ~length:1 () in
    Eq(Node.Buffer.length buf, 1)
  );

  "fromBuffer makes buffer from another Buffer", (fun _ -> 
    let source = Node.Buffer.fromString "abc" in
    let target = Node.Buffer.fromBuffer (`Buffer source) in
    Eq(Node.Buffer.toString target, "abc")
  );

  "fromString makes buffer from string value", (fun _ ->
    let buf = Node.Buffer.fromString "abc" in
    Eq(Node.Buffer.toString buf, "abc")
  );

  "fromStringWithEncoding supports specifying string encoding", (fun _ -> 
    let buf = Node.Buffer.fromStringWithEncoding "YWJj" `base64 in
    Eq(Node.Buffer.toString buf, "abc")
  );

  "isBuffer returns true for buffer values", (fun _ ->
    let buf = Node.Buffer.fromString "abc" in
    Ok(Node.Buffer.isBuffer buf)
  );

  "isBuffer returns false for non-buffer values", (fun _ -> 
    let value = "abc" in
    Ok(not (Node.Buffer.isBuffer value))
  );

  "isEncoding returns true for valid encoding strings", (fun _ -> 
    Ok(Node.Buffer.isEncoding "binary")
  );

  "isEncoding returns false for valid encoding string", (fun _ ->
    Ok(not (Node.Buffer.isEncoding "some-arbitraty-string"))
  );
    
  "poolSize returns internal Buffer pool size", (fun _ ->
    Eq(Js.typeof (Node.Buffer.poolSize), "number")
  );

  "unsafe_get allows direct buffer reading by position", (fun _ ->
    let buf = Node.Buffer.fromString "abc" in
    Eq(Node.Buffer.unsafe_get buf 0, 97)
  );

  "unsafe_set allows direct buffer modification by position", (fun _ ->
    let buf = Node.Buffer.fromString "abc" in
    let () = Node.Buffer.unsafe_set buf ~index:0 98 in
    Eq(Node.Buffer.toString buf, "bbc")
  );

  "buffer returns underlying ArrayBuffer", (fun _ ->
    let buf = Node.Buffer.fromString "abc" in
    let buf2 = Node.Buffer.fromString "bbc" in
    let ab = Node.Buffer.buffer buf in
    let ab2 = Node.Buffer.buffer buf2 in
    (* default allocation is buffered so two buffers share same ArrayBuffer *)
    Eq(ab, ab2)
  );

  "byteOffset reurns buffer offset in underlying ArrayBuffer", (fun _ -> 
    let buf = Node.Buffer.fromString "abc" in
    let offset = Node.Buffer.byteOffset buf in
    Eq(Js.typeof offset, "number")
  );

  "copy transfers one buffer region to another", (fun _ ->
    let buf = Node.Buffer.fromString "abc" in
    let buf2 = Node.Buffer.fromString "xxxxx" in
    let () = ignore (Node.Buffer.copy buf ~target:buf2 ~targetStart:1 ~sourceStart:1 ~sourceEnd:3 ()) in
    Eq(Node.Buffer.toString buf2, "xbcxx")
  );

  "equals returns true if two buffers share same bytes", (fun _ ->
    let buf = Node.Buffer.fromString "abc" in
    let buf2 = Node.Buffer.fromString "abc" in
    Ok(Node.Buffer.equals buf buf2)
  );

  "equals returns true if two buffers not share same bytes", (fun _ ->
    let buf = Node.Buffer.fromString "abc" in
    let buf2 = Node.Buffer.fromString "abd" in
    Ok(not (Node.Buffer.equals buf buf2))
  );

  "fill fills existing buffer with provided value", (fun _ ->
    let buf = Node.Buffer.fromString "aaaaa" in
    let buf = Node.Buffer.fill buf (`Integer 98) ~offset:2 ~end_:4 () in
    Eq(Node.Buffer.toString buf, "aabba")
  );

]

;; Mt.from_pair_suites __MODULE__ suites