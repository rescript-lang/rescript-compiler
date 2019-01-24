
let sequor b1 b2 =
  let b1 = ref b1 in
  let b2 = ref b2 in
  let b1 = !b1 in
  let b2 = !b2 in
  if (if b1 then true else b2 && if b1 then true else b2)
  then "true" else "false"

let sequand b1 b2 =
  let b1 = ref b1 in
  let b2 = ref b2 in
  let b1 = !b1 in
  let b2 = !b2 in
  if (if b1 then b2 else false && if b1 then b2 else false)
  then "true" else "false"

let sequor' b1 b2 =
  let b1 = ref b1 in
  let b2 = ref b2 in
  let b1 = !b1 in
  let b2 = !b2 in
  if (if b1 then true else b2 || if b1 then true else b2)
  then "true" else "false"

let sequand' b1 b2 =
  let b1 = ref b1 in
  let b2 = ref b2 in
  let b1 = !b1 in
  let b2 = !b2 in
  if (if b1 then b2 else false || if b1 then b2 else false)
  then "true" else "false"

let test b1 b2 =
  assert(sequor b1 b2 = if b1 || b2 then "true" else "false");
  assert(sequor' b1 b2 = if b1 || b2 then "true" else "false");
  assert(sequand b1 b2 = if b1 && b2 then "true" else "false");
  assert(sequand' b1 b2 = if b1 && b2 then "true" else "false")

let () =
  test false false;
  test false true;
  test true false;
  test true true
