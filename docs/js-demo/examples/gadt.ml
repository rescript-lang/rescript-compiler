

[%%bs.raw{|
  /*
   * this js function will work under both [string] and [float]
   */
  function add (x,y){
    return x + y;
  }
  |}]

type _ kind =
  | String : string kind
  | Float : float kind


external add : ('a kind [@bs.ignore]) -> 'a -> 'a -> 'a = "" [@@bs.val]

let () =
  Js.log (add Float 3.0 2.0);
  Js.log (add String "hello, "  "BuckleScript")
