(* Classic producer-consumer *)

type 'a prodcons =
  { buffer: 'a array;
    lock: Mutex.t;
    mutable readpos: int;
    mutable writepos: int;
    notempty: Condition.t;
    notfull: Condition.t }

let create size init =
  { buffer = Array.make size init;
    lock = Mutex.create();
    readpos = 0;
    writepos = 0;
    notempty = Condition.create();
    notfull = Condition.create() }

let put p data =
  Mutex.lock p.lock;
  while (p.writepos + 1) mod Array.length p.buffer = p.readpos do
    Condition.wait p.notfull p.lock
  done;
  p.buffer.(p.writepos) <- data;
  p.writepos <- (p.writepos + 1) mod Array.length p.buffer;
  Condition.signal p.notempty;
  Mutex.unlock p.lock

let get p =
  Mutex.lock p.lock;
  while p.writepos = p.readpos do
    Condition.wait p.notempty p.lock
  done;
  let data = p.buffer.(p.readpos) in
  p.readpos <- (p.readpos + 1) mod Array.length p.buffer;
  Condition.signal p.notfull;
  Mutex.unlock p.lock;
  data

(* Test *)

let rec produce buff n max =
  put buff n;
  if n < max then produce buff (n+1) max

let rec consume buff cur max =
  let n = get buff in
  if n <> cur then false
  else if n = max then true
  else consume buff (cur + 1) max

let _ =
  let buff1 = create 20 0 and buff2 = create 30 0 in
  let ok1 = ref false and ok2 = ref false in
  let _p1 = Thread.create (fun () -> produce buff1 0 10000) ()
  and _p2 = Thread.create (fun () -> produce buff2 0 8000) ()
  and c1 = Thread.create (fun () -> ok1 := consume buff1 0 10000) () in
  ok2 := consume buff2 0 8000;
  Thread.join c1;
  if !ok1 && !ok2
  then print_string "passed\n"
  else print_string "FAILED\n"
