
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
    Node.Buffer.copy buf ~target:buf2 ~targetStart:1 ~sourceStart:1 ~sourceEnd:3 () |. ignore;
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

  "fillWithString fills existing buffer with provided string", (fun _ ->
    let buf = Node.Buffer.fromString "xxxxx" in
    let buf = Node.Buffer.fillWithString buf "YWJj" ~encoding:`base64 ~offset:2 ~end_:4 () in
    Eq(Node.Buffer.toString buf, "xxabx")
  );

  "includes tests for presence of value starting from 'byteOffset' (value included)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Ok(Node.Buffer.includes buf (`Integer 99) ~byteOffset:3 ())
  );

  "includes tests for presence of value starting from 'byteOffset' (value included before offset)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Ok(not (Node.Buffer.includes buf (`Integer 99) ~byteOffset:4 ()))
  );

  "includesString tests for presence of value starting from 'byteOffset' (value included)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Ok(Node.Buffer.includesString buf "Yw==" ~byteOffset:3 ~encoding:`base64 ())
  );

  "includesString tests for presence of value starting from 'byteOffset' (value included before offset)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Ok(not (Node.Buffer.includesString buf "Yw==" ~byteOffset:4 ~encoding:`base64 ()))
  );

  "indexOf tests for presence of value starting from 'byteOffset' (value included)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Eq(Node.Buffer.indexOf buf (`Integer 99) ~byteOffset:3 (), 3)
  );

  "indexOf tests for presence of value starting from 'byteOffset' (value included before offset)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Eq(Node.Buffer.indexOf buf (`Integer 99) ~byteOffset:4 (), -1)
  );

  "indexOfString tests for presence of value starting from 'byteOffset' (value included)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Eq(Node.Buffer.indexOfString buf "Yw==" ~byteOffset:3 ~encoding:`base64 (), 3)
  );

  "indexOfString tests for presence of value starting from 'byteOffset' (value included before offset)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Eq(Node.Buffer.indexOfString buf "Yw==" ~byteOffset:4 ~encoding:`base64 (), -1)
  );

  "lastIndexOf tests for presence of value starting from 'byteOffset' (value included)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Eq(Node.Buffer.lastIndexOf buf (`Integer 99) ~byteOffset:3 (), 3)
  );

  "lastIndexOf tests for presence of value starting from 'byteOffset' (value included before offset)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Eq(Node.Buffer.lastIndexOf buf (`Integer 99) ~byteOffset:2 (), -1)
  );

  "lastIndexOfString tests for presence of value starting from 'byteOffset' (value included)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Eq(Node.Buffer.lastIndexOfString buf "Yw==" ~byteOffset:3 ~encoding:`base64 (), 3)
  );

  "lastIndexOfString tests for presence of value starting from 'byteOffset' (value included before offset)", (fun _ ->
    let buf = Node.Buffer.fromString "aabcbb" in
    Eq(Node.Buffer.lastIndexOfString buf "Yw==" ~byteOffset:2 ~encoding:`base64 (), -1)
  );

  "length returns buffer length in bytes", (fun _ -> 
    Eq("abc" |. Node.Buffer.fromString |. Node.Buffer.length, 3)
  );

  "subarray returns piece of buffer", (fun _ ->
    let buf = Node.Buffer.fromString "abcd" in
    Eq(Node.Buffer.toString (Node.Buffer.subarray buf ~start:1 ~end_:3 ()), "bc")
  );

  "slice returns piece of buffer", (fun _ ->
    let buf = Node.Buffer.fromString "abcd" in
    Eq(Node.Buffer.toString (Node.Buffer.slice buf ~start:1 ~end_:3 ()), "bc")
  );

  "swap16 swaps byte order as for int16", (fun _ ->
    let buf = [|1;2;3;4|]
      |. Node.Buffer.fromArray
      |. Node.Buffer.swap16 
    in
    Ok(
      (Node.Buffer.unsafe_get buf 0) = 2 &&
      (Node.Buffer.unsafe_get buf 1) = 1 &&
      (Node.Buffer.unsafe_get buf 2) = 4 &&
      (Node.Buffer.unsafe_get buf 3) = 3 
    )
  );

  "swap32 swaps byte order as for int32", (fun _ ->
    let buf = [|1;2;3;4|]
      |. Node.Buffer.fromArray
      |. Node.Buffer.swap32 
    in
    Ok(
      (Node.Buffer.unsafe_get buf 0) = 4 &&
      (Node.Buffer.unsafe_get buf 1) = 3 &&
      (Node.Buffer.unsafe_get buf 2) = 2 &&
      (Node.Buffer.unsafe_get buf 3) = 1 
    )
  );

  "swap64 swaps byte order as for int64", (fun _ ->
    let buf = [|1;2;3;4;5;6;7;8|]
      |. Node.Buffer.fromArray
      |. Node.Buffer.swap64
    in
    Ok(
      (Node.Buffer.unsafe_get buf 0) = 8 &&
      (Node.Buffer.unsafe_get buf 1) = 7 &&
      (Node.Buffer.unsafe_get buf 2) = 6 &&
      (Node.Buffer.unsafe_get buf 3) = 5 &&
      (Node.Buffer.unsafe_get buf 4) = 4 &&
      (Node.Buffer.unsafe_get buf 5) = 3 &&
      (Node.Buffer.unsafe_get buf 6) = 2 &&
      (Node.Buffer.unsafe_get buf 7) = 1 
    )
  );
  
  "toJSON", (fun _ ->
    let json = 
      [|1;2;3|]
      |. Node.Buffer.fromArray 
      |. Node.Buffer.toJSON in
    Eq(Js.Json.stringify json, "{\"type\":\"Buffer\",\"data\":[1,2,3]}")
  );

  "toString converts buffer to string", (fun _ -> 
    let source = "abc" in
    let target = source |. Node.Buffer.fromString |. Node.Buffer.toString in
    Eq(source, target)
  );

  "toString with encoding uses encoding for string conversion", (fun _ ->
    let source = "abc" in
    let target = source |. Node.Buffer.fromString |. Node.Buffer.toStringWithEncoding ~encoding:`base64 () in
    Eq(target, "YWJj")
  );

  "write", (fun _ -> 
    let buf = Node.Buffer.fromString "xxxxxxxx" in
    Node.Buffer.write buf "YWJj" ~encoding:`base64 ~offset:2 () |. ignore;
    Eq(Node.Buffer.toString buf, "xxabcxxx")
  );

  "writeLength", (fun _ -> 
    let buf = Node.Buffer.fromString "xxxxxxxx" in
    Node.Buffer.writeLength buf "YWJj" ~offset:2 ~encoding:`base64 ~length:2 () |. ignore;
    Eq(Node.Buffer.toString buf, "xxabxxxx")
  );

  "_INSPECT_MAX_BYTES", (fun _ ->
    Eq(Node.Buffer._INSPECT_MAX_BYTES |. Js.typeof, "number")
  );

  "kMaxLength", (fun _ ->
    Eq(Node.Buffer.kMaxLength |. Js.typeof, "number")
  );

  "transcode", (fun _ ->
    let buf = Node.Buffer.fromString {j|€|j} in
    let buf2 = Node.Buffer.transcode (`Buffer buf) ~fromEnc:`utf8 ~toEnc:`ascii in
    Eq(Node.Buffer.toString buf2, "?")
  );

  "_MAX_LENGTH", (fun _ -> 
    Eq(Node.Buffer._MAX_LENGTH |. Js.typeof, "number")
  );

  "_MAX_STRING_LENGTH", (fun _ -> 
    Eq(Node.Buffer._MAX_STRING_LENGTH |. Js.typeof, "number")
  );

]

;; Mt.from_pair_suites __MODULE__ suites