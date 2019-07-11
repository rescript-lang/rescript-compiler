let v = ref 0

(** Test when i is captured
    {[
    ((fun _ -> j = i; (fun _ -> v := !v + j)) ())
    ]}

    babel does this way:
    {[
var arr = []
var f = function(){
  for(let i = 0; i < 10 ; ++ i){
    arr [i] = ()=>{console.log(i)}
    
  }
}
    ]}

    {[
var arr = [];
var f = function f() {
  var _loop = function (i) {
    arr[i] = function () {
      console.log(i);
    };
  };

  for (var i = 0; i < 10; ++i) {
    _loop(i);
  }
};
    ]}

    This means inline is tricky in javascript, here we try to inline [_loop]? *)
let f () =
  let n = 10 in
  let arr = Array.make 10 (fun _ -> ()) in
  for i = 0 to n - 1 do
    arr.(i) <- (fun _ -> v := !v + i)
  done ;
  arr

let () =
  let u = f () in
  Array.iter (fun x -> x ()) u ;
  assert (!v = 45)
