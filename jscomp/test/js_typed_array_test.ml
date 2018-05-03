open Js_typed_array

external arrayFrom : 'a -> 'b array = "Array.from" [@@bs.val]

let mkI8 a = Int8Array.make a
let via make f arr = arrayFrom (f (make arr))
let viaInt8 f arr = via Int8Array.make


(* make sure we expose a type that's easy to refer to *)
let x: Int8Array.t = mkI8 [| 1; 2; 3 |]

let suites = Mt.[

(*  ArrayBuffer
 *)

    "array_buffer - make", (fun _ ->
      Eq(5, ArrayBuffer.make 5 |> ArrayBuffer.byteLength));

    (* experimental
    "array_buffer - transfer", (fun _ ->
      Eq(5, ArrayBuffer.make 5 |> ArrayBuffer.transfer |> ArrayBuffer.byteLength));
    "array_buffer - transferWithLength", (fun _ ->
      let a = ArrayBuffer.make 5 in
      Eq(10,  ArrayBuffer.transferWithLength a 10 |> ArrayBuffer.byteLength));
    *)

    "array_buffer - byteLength", (fun _ ->
      Eq(5, ArrayBuffer.make 5 |> ArrayBuffer.byteLength));

    "array_buffer - slice", (fun _ ->
      Eq(2, ArrayBuffer.make 5 |> ArrayBuffer.slice ~start:2 ~end_:4 |> ArrayBuffer.byteLength));
    "array_buffer - sliceFrom", (fun _ ->
      Eq(3, ArrayBuffer.make 5 |> ArrayBuffer.sliceFrom 2 |> ArrayBuffer.byteLength));

(* Generic typed array
*)

    "typed_array - unsafe_get", (fun _ ->
      Eq(4, Int8Array.unsafe_get (mkI8 [| 1; 2; 3; 4; 5 |]) 3));

    "typed_array - unsafe_set", (fun _ ->
      let a = mkI8 [| 1; 2; 3; 4; 5 |] in
      let _ = Int8Array.unsafe_set a 3 14 in

      Eq(14, Int8Array.unsafe_get a 3)
    );

    "typed_array - buffer", (fun _ ->
      Eq(mkI8 [| 3; 4; 5 |],
         mkI8 [| 1; 2; 3; 4; 5 |]
         |> Int8Array.buffer
         |> (function a -> Int8Array.fromBufferOffset a 2))
    );
    "typed_array - byteLength", (fun _ ->
      Eq(10, Int16Array.make [| 1; 2; 3; 4; 5 |] |> Int16Array.byteLength));
    "typed_array - byteOffset", (fun _ ->
      Eq(0, mkI8 [| 1; 2; 3; 4; 5 |] |> Int8Array.byteOffset));


    "typed_array - setArray", (fun _ ->
      let f a = Int8Array.setArray [| 9; 8; 7 |] a; a in

      Eq(mkI8 [| 9; 8; 7; 4; 5 |],
         mkI8 [| 1; 2; 3; 4; 5 |] |> f));

    "typed_array - setArrayOffset", (fun _ ->
      let f a = Int8Array.setArrayOffset [| 9; 8; 7 |] 2 a; a in

      Eq(mkI8 [| 1; 2; 9; 8; 7 |],
         mkI8 [| 1; 2; 3; 4; 5 |] |> f));

    (* These shouldn't compile
    "type_safe_sanity_check 1", (fun _ ->
      let a = Float32Array.make [| 1.; 2.; 3.; 4.; 5. |] in
      let _ = Int8Array.set a 3 14 in

      Eq(14, Int8Array.get a 3)
    );
    "type_safe_sanity_check 2", (fun _ ->
      let a = Int16Array.make [| 1; 2; 3; 4; 5 |] in
      let _ = Int8Array.set a 3 14 in

      Eq(14, Int8Array.get a 3)
    );
    *)

    (* Array itnerface(-ish) *)

    "typed_array - length", (fun _ ->
      Eq(5, mkI8 [| 1; 2; 3; 4; 5 |] |> Int8Array.length)
    );

    "typed_array - copyWithin", (fun _ ->
      Eq(mkI8 [| 1; 2; 3; 1; 2 |],
         mkI8 [| 1; 2; 3; 4; 5 |] |> Int8Array.copyWithin ~to_:(-2))
    );
    "typed_array - copyWithinFrom", (fun _ ->
      Eq(mkI8 [| 4; 5; 3; 4; 5 |],
         mkI8 [| 1; 2; 3; 4; 5 |] |> Int8Array.copyWithinFrom ~to_:0 ~from:3)
    );
    "typed_array - copyWithinFromRange", (fun _ ->
      Eq(mkI8 [| 4; 2; 3; 4; 5 |],
         mkI8 [| 1; 2; 3; 4; 5 |]
         |> Int8Array.copyWithinFromRange ~to_:0 ~start:3 ~end_:4)
    );

    "typed_array - fillInPlace", (fun _ ->
      Eq(mkI8 [| 4; 4; 4 |],
         mkI8 [| 1; 2; 3 |] |> Int8Array.fillInPlace 4)
    );
    "typed_array - fillFromInPlace", (fun _ ->
      Eq(mkI8 [| 1; 4; 4 |],
         mkI8 [| 1; 2; 3 |] |> Int8Array.fillFromInPlace 4 ~from:1)
    );
    "typed_array - fillRangeInPlace", (fun _ ->
      Eq(mkI8 [| 1; 4; 3 |],
         mkI8 [| 1; 2; 3 |] |> Int8Array.fillRangeInPlace 4 ~start:1 ~end_:2)
    );

    "typed_array - reverseInPlace", (fun _ ->
      Eq(mkI8 [| 3; 2; 1 |],
         mkI8 [| 1; 2; 3 |] |> Int8Array.reverseInPlace)
    );

    "typed_array - sortInPlace", (fun _ ->
      Eq(mkI8 [| 1; 2; 3 |],
         mkI8 [| 3; 1; 2 |] |> Int8Array.sortInPlace)
    );
    "typed_array - sortInPlaceWith", (fun _ ->
      Eq(mkI8 [| 3; 2; 1 |],
         mkI8 [| 3; 1; 2 |]
         |> Int8Array.sortInPlaceWith ((fun a b -> b - a) [@bs]))
    );

    (* es2016 *)
    "typed_array - includes", (fun _ ->
      Eq(true, mkI8 [| 1; 2; 3 |] |> Int8Array.includes 3)
    );

    "typed_array - indexOf", (fun _ ->
      Eq(1, mkI8 [| 1; 2; 3 |] |> Int8Array.indexOf 2)
    );
    "typed_array - indexOfFrom", (fun _ ->
      Eq(3, mkI8 [| 1; 2; 3; 2 |] |> Int8Array.indexOfFrom 2 ~from:2)
    );

    "typed_array - join", (fun _ ->
      Eq("1,2,3", mkI8 [| 1; 2; 3 |] |> Int8Array.join)
    );
    "typed_array - joinWith", (fun _ ->
      Eq("1;2;3", mkI8 [| 1; 2; 3 |] |> Int8Array.joinWith ";")
    );

    "typed_array - lastIndexOf", (fun _ ->
      Eq(1, mkI8 [| 1; 2; 3 |] |> Int8Array.lastIndexOf 2)
    );
    "typed_array - lastIndexOfFrom", (fun _ ->
      Eq(1, mkI8 [| 1; 2; 3; 2 |] |> Int8Array.lastIndexOfFrom 2 ~from:2)
    );

    "typed_array - slice", (fun _ ->
      Eq(mkI8 [| 2; 3; |],
         mkI8 [| 1; 2; 3; 4; 5 |] |> Int8Array.slice ~start:1 ~end_:3)
    );
    "typed_array - copy", (fun _ ->
      Eq(mkI8 [| 1; 2; 3; 4; 5 |],
         mkI8 [| 1; 2; 3; 4; 5 |] |> Int8Array.copy)
    );
    "typed_array - sliceFrom", (fun _ ->
      Eq(mkI8 [| 3; 4; 5 |],
         mkI8 [| 1; 2; 3; 4; 5 |] |> Int8Array.sliceFrom 2)
    );

    "typed_array - subarray", (fun _ ->
      Eq(mkI8 [| 2; 3; |],
         mkI8 [| 1; 2; 3; 4; 5 |] |> Int8Array.subarray ~start:1 ~end_:3)
    );
    "typed_array - subarrayFrom", (fun _ ->
      Eq(mkI8 [| 3; 4; 5 |],
         mkI8 [| 1; 2; 3; 4; 5 |] |> Int8Array.subarrayFrom 2)
    );

    "typed_array - toString", (fun _ ->
      Eq("1,2,3", mkI8 [| 1; 2; 3 |] |> Int8Array.toString)
    );
    "typed_array - toLocaleString", (fun _ ->
      Eq("1,2,3", mkI8 [| 1; 2; 3 |] |> Int8Array.toLocaleString)
    );

    (* es2015, iterator
    "typed_array - entries", (fun _ ->
      Eq(mkI8 [| (0, "a"); (1, "b"); (2, "c") |],
         mkI8 [| "a"; "b"; "c" |] |> Int8Array.entries |> Int8Array.from)
    );
    *)

    "typed_array - every", (fun _ ->
      Eq(true,
         mkI8 [| 1; 2; 3 |]
         |> Int8Array.every ((fun n -> (n > 0)) [@bs]))
    );
    "typed_array - everyi", (fun _ ->
      Eq(false,
         mkI8 [| 1; 2; 3 |]
         |> Int8Array.everyi ((fun _ i -> (i > 0)) [@bs]))
    );

    "typed_array - filter", (fun _ ->
      Eq(mkI8 [| 2; 4 |],
         mkI8 [| 1; 2; 3; 4 |]
         |> Int8Array.filter ((fun n -> n mod 2 = 0) [@bs]))
    );
    "typed_array - filteri", (fun _ ->
      Eq(mkI8 [| 1; 3 |],
         mkI8 [| 1; 2; 3; 4 |]
         |> Int8Array.filteri ((fun _ i -> (i mod 2 = 0)) [@bs]))
    );

    "typed_array - find", (fun _ ->
      Eq(Js.Undefined.return 2,
         mkI8 [| 1; 2; 3; 4 |]
         |> Int8Array.find ((fun n -> n mod 2 = 0) [@bs]))
    );
    "typed_array - findi", (fun _ ->
      Eq(Js.Undefined.return 1,
         mkI8 [| 1; 2; 3; 4 |]
         |> Int8Array.findi ((fun _ i -> i mod 2 = 0) [@bs]))
    );

    "typed_array - findIndex", (fun _ ->
      Eq(1, mkI8 [| 1; 2; 3; 4 |]
        |> Int8Array.findIndex ((fun n -> n mod 2 = 0) [@bs]))
    );
    "typed_array - findIndexi", (fun _ ->
      Eq(0, mkI8 [| 1; 2; 3; 4 |]
        |> Int8Array.findIndexi ((fun _ i -> i mod 2 = 0) [@bs]))
    );

    "typed_array - forEach", (fun _ ->
      let sum = ref 0 in
      let _ = mkI8 [| 1; 2; 3; |]
        |> Int8Array.forEach ((fun n -> sum := !sum + n) [@bs]) in

      Eq(6, !sum)
    );
    "typed_array - forEachi", (fun _ ->
      let sum = ref 0 in
      let _ = mkI8 [| 1; 2; 3; |]
        |> Int8Array.forEachi ((fun _ i -> sum := !sum + i) [@bs]) in

      Eq(3, !sum)
    );

    (* es2015, iterator
    "typed_array - keys", (fun _ ->
      Eq(mkI8 [| 0; 1; 2 |],
         mkI8 [| "a"; "b"; "c" |] |> Int8Array.keys |> Int8Array.from)
    );
    *)

    "typed_array - map", (fun _ ->
      Eq(mkI8 [| 2; 4; 6; 8 |],
         mkI8 [| 1; 2; 3; 4 |] |> Int8Array.map ((fun n -> n * 2) [@bs]))
    );
    "typed_array - map", (fun _ ->
      Eq(mkI8 [| 0; 2; 4; 6 |],
         mkI8 [| 1; 2; 3; 4 |] |> Int8Array.mapi ((fun _ i -> i * 2) [@bs]))
    );

    "typed_array - reduce", (fun _ ->
      Eq(-10,
         mkI8 [| 1; 2; 3; 4 |]
         |> Int8Array.reduce ((fun acc n -> acc - n) [@bs]) 0)
    );
    "typed_array - reducei", (fun _ ->
      Eq(-6,
         mkI8 [| 1; 2; 3; 4 |]
         |> Int8Array.reducei ((fun acc _ i -> acc - i) [@bs]) 0)
    );

    "typed_array - reduceRight", (fun _ ->
      Eq(-10,
         mkI8 [| 1; 2; 3; 4 |]
         |> Int8Array.reduceRight ((fun acc n -> acc - n) [@bs]) 0)
    );
    "typed_array - reduceRighti", (fun _ ->
      Eq(-6,
         mkI8 [| 1; 2; 3; 4 |]
         |> Int8Array.reduceRighti ((fun acc _ i -> acc - i) [@bs]) 0)
    );

    "typed_array - some", (fun _ ->
      Eq(false,
         mkI8 [| 1; 2; 3; 4 |]
         |> Int8Array.some ((fun n -> (n <= 0)) [@bs]))
    );
    "typed_array - somei", (fun _ ->
      Eq(true,
         mkI8 [| 1; 2; 3; 4 |]
         |> Int8Array.somei ((fun _ i -> (i <= 0)) [@bs]))
    );

    (* es2015, iterator
    "typed_array - values", (fun _ ->
      Eq(mkI8 [| "a"; "b"; "c" |],
         mkI8 [| "a"; "b"; "c" |] |> Int8Array.values |> Int8Array.from)
    );
    *)

(*  Int8Array
 *)

    "int8_array - _BYTES_PER_ELEMENT", (fun _ ->
      Eq(1, Int8Array._BYTES_PER_ELEMENT));

    (* byte length is a decent indicator of the kind of typed array *)
    "int8_array - make", (fun _ ->
      Eq(3, Int8Array.make [| 1; 2; 3 |] |> Int8Array.byteLength));
    "int8_array - fromBuffer", (fun _ ->
      Eq(32, ArrayBuffer.make 32 |> Int8Array.fromBuffer |> Int8Array.byteLength));
    "int8_array - fromBufferOffset", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(24, Int8Array.fromBufferOffset buffer 8 |> Int8Array.byteLength));
    "int8_array - fromBufferRange", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(2, Int8Array.fromBufferRange buffer ~offset:8 ~length:2 |> Int8Array.byteLength));
    "int8_array - fromLength", (fun _ ->
      Eq(3, Int8Array.fromLength 3 |> Int8Array.byteLength));
    (* unable to test because nothing currently implements array_like
    "int8_array - from", (fun _ ->
      Eq(3, [| "a"; "b"; "c" |] |> Js.Array.keys |> Int8Array.from |> Int8Array.byteLength));
    *)
    "int8_array - unsafe_set - typed_array sanity check", (fun _ ->
      let a = Int8Array.make [| 1; 2; 3; 4; 5 |] in
      let _ = Int8Array.unsafe_set a 3 14 in

      Eq(14, Int8Array.unsafe_get a 3)
    );

(*  Uint8Array
 *)

    "uint8_array - _BYTES_PER_ELEMENT", (fun _ ->
      Eq(1, Uint8Array._BYTES_PER_ELEMENT));

    (* byte length is a decent indicator of the kind of typed array *)
    "uint8_array - make", (fun _ ->
      Eq(3, Uint8Array.make [| 1; 2; 3 |] |> Uint8Array.byteLength));
    "uint8_array - fromBuffer", (fun _ ->
      Eq(32, ArrayBuffer.make 32 |> Uint8Array.fromBuffer |> Uint8Array.byteLength));
    "uint8_array - fromBufferOffset", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(24, Uint8Array.fromBufferOffset buffer 8 |> Uint8Array.byteLength));
    "uint8_array - fromBufferRange", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(2, Uint8Array.fromBufferRange buffer ~offset:8 ~length:2 |> Uint8Array.byteLength));
    "uint8_array - fromLength", (fun _ ->
      Eq(3, Uint8Array.fromLength 3 |> Uint8Array.byteLength));
    (* unable to test because nothing currently implements array_like
    "uint8_array - from", (fun _ ->
      Eq(3, [| "a"; "b"; "c" |] |> Js.Array.keys |> Uint8Array.from |> Uint8Array.byteLength));
    *)
    "uint8_array - unsafe_set - typed_array sanity check", (fun _ ->
      let a = Uint8Array.make [| 1; 2; 3; 4; 5 |] in
      let _ = Uint8Array.unsafe_set a 3 14 in

      Eq(14, Uint8Array.unsafe_get a 3)
    );

(*  Uint8ClampedArray
 *)

    "uint8clamped_array - _BYTES_PER_ELEMENT", (fun _ ->
      Eq(1, Uint8ClampedArray._BYTES_PER_ELEMENT));

    (* byte length is a decent indicator of the kind of typed array *)
    "uint8clamped_array - make", (fun _ ->
      Eq(3, Uint8ClampedArray.make [| 1; 2; 3 |] |> Uint8ClampedArray.byteLength));
    "uint8clamped_array - fromBuffer", (fun _ ->
      Eq(32, ArrayBuffer.make 32 |> Uint8ClampedArray.fromBuffer |> Uint8ClampedArray.byteLength));
    "uint8clamped_array - fromBufferOffset", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(24, Uint8ClampedArray.fromBufferOffset buffer 8 |> Uint8ClampedArray.byteLength));
    "uint8clamped_array - fromBufferRange", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(2, Uint8ClampedArray.fromBufferRange buffer ~offset:8 ~length:2 |> Uint8ClampedArray.byteLength));
    "uint8clamped_array - fromLength", (fun _ ->
      Eq(3, Uint8ClampedArray.fromLength 3 |> Uint8ClampedArray.byteLength));
    (* unable to test because nothing currently implements array_like
    "uint8clamped_array - from", (fun _ ->
      Eq(3, [| "a"; "b"; "c" |] |> Js.Array.keys |> Uint8ClampedArray.from |> Uint8ClampedArray.byteLength));
    *)
    "uint8clamped_array - unsafe_set - typed_array sanity check", (fun _ ->
      let a = Uint8ClampedArray.make [| 1; 2; 3; 4; 5 |] in
      let _ = Uint8ClampedArray.unsafe_set a 3 14 in

      Eq(14, Uint8ClampedArray.unsafe_get a 3)
    );

(*  Int16Array
 *)

    "int16_array - _BYTES_PER_ELEMENT", (fun _ ->
      Eq(2, Int16Array._BYTES_PER_ELEMENT));

    (* byte length is a decent indicator of the kind of typed array *)
    "int16_array - make", (fun _ ->
      Eq(6, Int16Array.make [| 1; 2; 3 |] |> Int16Array.byteLength));
    "int16_array - fromBuffer", (fun _ ->
      Eq(32, ArrayBuffer.make 32 |> Int16Array.fromBuffer |> Int16Array.byteLength));
    "int16_array - fromBufferOffset", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(24, Int16Array.fromBufferOffset buffer 8 |> Int16Array.byteLength));
    "int16_array - fromBufferRange", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(4, Int16Array.fromBufferRange buffer ~offset:8 ~length:2 |> Int16Array.byteLength));
    "int16_array - fromLength", (fun _ ->
      Eq(6, Int16Array.fromLength 3 |> Int16Array.byteLength));
    (* unable to test because nothing currently implements array_like
    "int16_array - from", (fun _ ->
      Eq(3, [| "a"; "b"; "c" |] |> Js.Array.keys |> Int16Array.from |> Int16Array.byteLength));
    *)
    "int16_array - unsafe_set - typed_array sanity check", (fun _ ->
      let a = Int16Array.make [| 1; 2; 3; 4; 5 |] in
      let _ = Int16Array.unsafe_set a 3 14 in

      Eq(14, Int16Array.unsafe_get a 3)
    );

(*  Uint16Array
 *)

    "uint16_array - _BYTES_PER_ELEMENT", (fun _ ->
      Eq(2, Uint16Array._BYTES_PER_ELEMENT));

    (* byte length is a decent indicator of the kind of typed array *)
    "uint16_array - make", (fun _ ->
      Eq(6, Uint16Array.make [| 1; 2; 3 |] |> Uint16Array.byteLength));
    "uint16_array - fromBuffer", (fun _ ->
      Eq(32, ArrayBuffer.make 32 |> Uint16Array.fromBuffer |> Uint16Array.byteLength));
    "uint16_array - fromBufferOffset", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(24, Uint16Array.fromBufferOffset buffer 8 |> Uint16Array.byteLength));
    "uint16_array - fromBufferRange", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(4, Uint16Array.fromBufferRange buffer ~offset:8 ~length:2 |> Uint16Array.byteLength));
    "uint16_array - fromLength", (fun _ ->
      Eq(6, Uint16Array.fromLength 3 |> Uint16Array.byteLength));
    (* unable to test because nothing currently implements array_like
    "uint16_array - from", (fun _ ->
      Eq(3, [| "a"; "b"; "c" |] |> Js.Array.keys |> Uint16Array.from |> Uint16Array.byteLength));
    *)
    "uint16_array - unsafe_set - typed_array sanity check", (fun _ ->
      let a = Uint16Array.make [| 1; 2; 3; 4; 5 |] in
      let _ = Uint16Array.unsafe_set a 3 14 in

      Eq(14, Uint16Array.unsafe_get a 3)
    );

(*  Int32Array
 *)

    "int32_array - _BYTES_PER_ELEMENT", (fun _ ->
      Eq(4, Int32Array._BYTES_PER_ELEMENT));

    (* byte length is a decent indicator of the kind of typed array *)
    "int32_array - make", (fun _ ->
      Eq(12, Int32Array.make ([| 1; 2; 3 |] |> Array.map Int32.of_int) |> Int32Array.byteLength));
    "int32_array - fromBuffer", (fun _ ->
      Eq(32, ArrayBuffer.make 32 |> Int32Array.fromBuffer |> Int32Array.byteLength));
    "int32_array - fromBufferOffset", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(24, Int32Array.fromBufferOffset buffer 8 |> Int32Array.byteLength));
    "int32_array - fromBufferRange", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(8, Int32Array.fromBufferRange buffer ~offset:8 ~length:2 |> Int32Array.byteLength));
    "int32_array - fromLength", (fun _ ->
      Eq(12, Int32Array.fromLength 3 |> Int32Array.byteLength));
    (* unable to test because nothing currently implements array_like
    "int32_array - from", (fun _ ->
      Eq(3, [| "a"; "b"; "c" |] |> Js.Array.keys |> Int32Array.from |> Int32Array.byteLength));
    *)
    "int32_array - unsafe_set - typed_array sanity check", (fun _ ->
      let a = Int32Array.make ([| 1; 2; 3; 4; 5 |] |> Array.map Int32.of_int) in
      let _ = Int32Array.unsafe_set a 3 (Int32.of_int 14) in

      Eq(Int32.of_int 14, Int32Array.unsafe_get a 3)
    );

(*  Uint32Array
 *)

    "uint32_array - _BYTES_PER_ELEMENT", (fun _ ->
      Eq(4, Uint32Array._BYTES_PER_ELEMENT));

    (* byte length is a decent indicator of the kind of typed array *)
    "uint32_array - make", (fun _ ->
      Eq(12, Uint32Array.make [| 1; 2; 3 |] |> Uint32Array.byteLength));
    "uint32_array - fromBuffer", (fun _ ->
      Eq(32, ArrayBuffer.make 32 |> Uint32Array.fromBuffer |> Uint32Array.byteLength));
    "uint32_array - fromBufferOffset", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(24, Uint32Array.fromBufferOffset buffer 8 |> Uint32Array.byteLength));
    "uint32_array - fromBufferRange", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(8, Uint32Array.fromBufferRange buffer ~offset:8 ~length:2 |> Uint32Array.byteLength));
    "uint32_array - fromLength", (fun _ ->
      Eq(12, Uint32Array.fromLength 3 |> Uint32Array.byteLength));
    (* unable to test because nothing currently implements array_like
    "uint32_array - from", (fun _ ->
      Eq(3, [| "a"; "b"; "c" |] |> Js.Array.keys |> Uint32Array.from |> Uint32Array.byteLength));
    *)
    "uint32_array - unsafe_set - typed_array sanity check", (fun _ ->
      let a = Uint32Array.make [| 1; 2; 3; 4; 5 |] in
      let _ = Uint32Array.unsafe_set a 3 14 in

      Eq(14, Uint32Array.unsafe_get a 3)
    );

(*  Float32Array
 *)

    "float32_array - _BYTES_PER_ELEMENT", (fun _ ->
      Eq(4, Float32Array._BYTES_PER_ELEMENT));

    (* byte length is a decent indicator of the kind of typed array *)
    "float32_array - make", (fun _ ->
      Eq(12, Float32Array.make [| 1.; 2.; 3. |] |> Float32Array.byteLength));
    "float32_array - fromBuffer", (fun _ ->
      Eq(32, ArrayBuffer.make 32 |> Float32Array.fromBuffer |> Float32Array.byteLength));
    "float32_array - fromBufferOffset", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(24, Float32Array.fromBufferOffset buffer 8 |> Float32Array.byteLength));
    "float32_array - fromBufferRange", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(8, Float32Array.fromBufferRange buffer ~offset:8 ~length:2 |> Float32Array.byteLength));
    "float32_array - fromLength", (fun _ ->
      Eq(12, Float32Array.fromLength 3 |> Float32Array.byteLength));
    (* unable to test because nothing currently implements array_like
    "float32_array - from", (fun _ ->
      Eq(3, [| "a"; "b"; "c" |] |> Js.Array.keys |> Float32Array.from |> Float32Array.byteLength));
    *)
    "float32_array - unsafe_set - typed_array sanity check", (fun _ ->
      let a = Float32Array.make [| 1.; 2.; 3.; 4.; 5. |] in
      let _ = Float32Array.unsafe_set a 3 14. in

      Eq(14., Float32Array.unsafe_get a 3)
    );

(*  Float64Array
 *)

    "float64_array - _BYTES_PER_ELEMENT", (fun _ ->
      Eq(8, Float64Array._BYTES_PER_ELEMENT));

    (* byte length is a decent indicator of the kind of typed array *)
    "float64_array - make", (fun _ ->
      Eq(24, Float64Array.make [| 1.; 2.; 3. |] |> Float64Array.byteLength));
    "float64_array - fromBuffer", (fun _ ->
      Eq(32, ArrayBuffer.make 32 |> Float64Array.fromBuffer |> Float64Array.byteLength));
    "float64_array - fromBufferOffset", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(24, Float64Array.fromBufferOffset buffer 8 |> Float64Array.byteLength));
    "float64_array - fromBufferRange", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(16, Float64Array.fromBufferRange buffer ~offset:8 ~length:2 |> Float64Array.byteLength));
    "float64_array - fromLength", (fun _ ->
      Eq(24, Float64Array.fromLength 3 |> Float64Array.byteLength));
    (* unable to test because nothing currently implements array_like
    "float64_array - from", (fun _ ->
      Eq(3, [| "a"; "b"; "c" |] |> Js.Array.keys |> Float64Array.from |> Float64Array.byteLength));
    *)
    "float64_array - unsafe_set - typed_array sanity check", (fun _ ->
      let a = Float64Array.make [| 1.; 2.; 3.; 4.; 5. |] in
      let _ = Float64Array.unsafe_set a 3 14. in

      Eq(14., Float64Array.unsafe_get a 3)
    );

(*  DataView
 *)

    "DataView - make, byteLength", (fun _ ->
      Eq(32, ArrayBuffer.make 32 |> DataView.make |> DataView.byteLength));
    "DataView - fromBuffer", (fun _ ->
      Eq(32, ArrayBuffer.make 32 |> DataView.fromBuffer |> DataView.byteLength));
    "DataView - fromBufferOffset", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(24, DataView.fromBufferOffset buffer 8 |> DataView.byteLength));
    "DataView - fromBufferRange", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(4, DataView.fromBufferRange buffer ~offset:8 ~length:4 |> DataView.byteLength));
    "DataView - buffer", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(buffer, DataView.fromBuffer buffer |> DataView.buffer));
    "DataView - byteOffset", (fun _ ->
      let buffer = ArrayBuffer.make 32 in
      Eq(8, DataView.fromBufferOffset buffer 8 |> DataView.byteOffset));

    "DataView - setInt8, getInt8", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setInt8 view 0 1;
      Eq(1, DataView.getInt8 view 0));
    "DataView - setUint8, getUint8", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setUint8 view 0 128;
      Eq(128, DataView.getUint8 view 0));

    "DataView - setInt16, getInt16", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setInt16 view 0 257;
      Eq(257, DataView.getInt16 view 0));
    "DataView - getInt16LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setInt16LittleEndian view 0 25000;
      Eq(25000, DataView.getInt16LittleEndian view 0));
    "DataView - setInt16LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setInt16LittleEndian view 0 25000;
      Eq(-22431, DataView.getInt16 view 0));

    "DataView - setUint16, getUint16", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setUint16 view 0 32768;
      Eq(32768, DataView.getUint16 view 0));
    "DataView - getUint16LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setUint16LittleEndian view 0 32768;
      Eq(32768, DataView.getUint16LittleEndian view 0));
    "DataView - setUint16LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setUint16LittleEndian view 0 32768;
      Eq(128, DataView.getUint16 view 0));

    "DataView - setInt32, getInt32", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setInt32 view 0 65537;
      Eq(65537, DataView.getInt32 view 0));
    "DataView - getInt32LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setInt32LittleEndian view 0 65537;
      Eq(65537, DataView.getInt32LittleEndian view 0));
    "DataView - setInt32LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setInt32LittleEndian view 0 65537;
      Eq(16777472, DataView.getInt32 view 0));

    (* Testing against 2_147_483_649 would be better,
       but JS platform restrict us with int32 *)
    "DataView - setUint32, getUint32", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setUint32 view 0 65537;
      Eq(65537, DataView.getUint32 view 0));
    "DataView - getUint32LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setUint32LittleEndian view 0 65537;
      Eq(65537, DataView.getUint32LittleEndian view 0));
    "DataView - setUint32LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setUint32LittleEndian view 0 65537;
      Eq(16777472, DataView.getUint32 view 0));

    "DataView - setFloat32, getFloat32", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setFloat32 view 0 65537.0;
      Eq(65537.0, DataView.getFloat32 view 0));
    "DataView - getFloat32LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setFloat32LittleEndian view 0 65537.0;
      Eq(65537.0, DataView.getFloat32LittleEndian view 0));
    "DataView - setFloat32LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setFloat32LittleEndian view 0 1.0;
      Eq(4.600602988224807e-41, DataView.getFloat32 view 0));

    "DataView - setFloat64, getFloat64", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setFloat64 view 0 1e200;
      Eq(1e200, DataView.getFloat64 view 0));
    "DataView - getFloat64LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setFloat64LittleEndian view 0 1e200;
      Eq(1e200, DataView.getFloat64LittleEndian view 0));
    "DataView - setFloat64LittleEndian", (fun _ ->
      let buffer = ArrayBuffer.make 8 in
      let view = DataView.make buffer in
      DataView.setFloat64LittleEndian view 0 1.0;
      Eq(3.03865e-319, DataView.getFloat64 view 0));
]

;; Mt.from_pair_suites __FILE__ suites
