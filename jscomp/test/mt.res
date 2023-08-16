@val external describe: (string, (. unit) => unit) => unit = "describe"

@val external it: (string, @uncurry (unit => unit)) => unit = "it"

@val external it_promise: (string, @uncurry (unit => Js.Promise.t<_>)) => unit = "it"

@val @module("assert") external eq: ('a, 'a) => unit = "deepEqual"

@val @module("assert") external neq: ('a, 'a) => unit = "notDeepEqual"

@val @module("assert") external strict_eq: ('a, 'a) => unit = "strictEqual"

@val @module("assert") external strict_neq: ('a, 'a) => unit = "notStrictEqual"

@val @module("assert") external ok: bool => unit = "ok"

@val @module("assert") external fail: ('a, 'a, Js.undefined<string>, string) => unit = "fail"

@val @variadic external dump: array<'a> => unit = "console.log"

@val
@module("assert")
/** There is a problem --
    it does not return [unit]
*/
external throws: (unit => unit) => unit = "throws"

let assert_equal = eq
let assert_notequal = neq
let assert_strict_equal = strict_eq
let assert_strict_notequal = strict_neq
let assert_ok = a => ok(a)
let assert_fail = msg => fail((), (), Js.Undefined.return(msg), "")

@module("process") external argv: array<string> = "argv"
@module("path") external basename: string => string = "basename"

let is_mocha = () =>
  switch Array.to_list(argv) {
  | list{_node, mocha, ..._} =>
    let exec = basename(mocha)
    exec == "mocha" || exec == "_mocha"
  | _ => false
  }
/* assert -- raises an AssertionError which mocha handls better
 */

let from_suites = %raw(`
function from_suites(name, suite) {
  var match = $$Array.to_list(Process.argv);
  if (match && is_mocha(undefined)) {
    describe(name, (function () {
            return List.iter((function (param) {
                          var partial_arg = param[1];
                          it(param[0], (function () {
                                  return Curry._1(partial_arg, undefined);
                                }));
                        }), suite);
          }));
    return ;
  }
  
}
`)

type rec eq =
  | Eq('a, 'a): eq
  | Neq('a, 'a): eq
  | StrictEq('a, 'a): eq
  | StrictNeq('a, 'a): eq
  | Ok(bool): eq
  | Approx(float, float): eq
  | ApproxThreshold(float, float, float): eq
  | ThrowAny(unit => unit): eq
  | Fail(unit): eq
  | FailWith(string): eq
/* TODO: | Exception : exn -> (unit -> unit) -> _ eq */

type pair_suites = list<(string, unit => eq)>
type promise_suites = list<(string, Js.Promise.t<eq>)>
let close_enough = (~threshold=0.0000001 /* epsilon_float */, a, b) => abs_float(a -. b) < threshold

let node_from_pair_suites = (name: string, suites: pair_suites) => {
  Js.log((name, "testing"))
  List.iter(((name, code)) =>
    switch code() {
    | Eq(a, b) => Js.log((name, a, "eq?", b))
    | Neq(a, b) => Js.log((name, a, "neq?", b))
    | StrictEq(a, b) => Js.log((name, a, "strict_eq?", b))
    | StrictNeq(a, b) => Js.log((name, a, "strict_neq?", b))
    | Approx(a, b) => Js.log((name, a, "~", b))
    | ApproxThreshold(t, a, b) => Js.log((name, a, "~", b, " (", t, ")"))
    | ThrowAny(fn) => ()
    | Fail(_) => Js.log("failed")
    | FailWith(msg) => Js.log("failed: " ++ msg)
    | Ok(a) => Js.log((name, a, "ok?"))
    }
  , suites)
}

let handleCode = spec =>
  switch spec {
  | Eq(a, b) => assert_equal(a, b)
  | Neq(a, b) => assert_notequal(a, b)
  | StrictEq(a, b) => assert_strict_equal(a, b)
  | StrictNeq(a, b) => assert_strict_notequal(a, b)
  | Ok(a) => assert_ok(a)
  | Approx(a, b) =>
    if !close_enough(a, b) {
      assert_equal(a, b)
    } /* assert_equal gives better ouput */
  | ApproxThreshold(t, a, b) =>
    if !close_enough(~threshold=t, a, b) {
      assert_equal(a, b)
    } /* assert_equal gives better ouput */
  | ThrowAny(fn) => throws(fn)
  | Fail(_) => assert_fail("failed")
  | FailWith(msg) => assert_fail(msg)
  }

let force_curry = x => {
  let _ = List.hd(list{3})
  let _ = Array.copy([5])
  x()
}
let from_pair_suites = %raw(`
function from_pair_suites(name, suites) {
  var match = $$Array.to_list(Process.argv);
  if (match) {
    if (is_mocha(undefined)) {
      describe(name, (function () {
              return List.iter((function (param) {
                            var code = param[1];
                            it(param[0], (function () {
                                    return handleCode(Curry._1(code, undefined));
                                  }));
                          }), suites);
            }));
      return ;
    } else {
      console.log([
            name,
            "testing"
          ]);
      return List.iter((function (param) {
                    var name = param[0];
                    var fn = Curry._1(param[1], undefined);
                    switch (fn.TAG) {
                      case "Eq" :
                          console.log([
                                name,
                                fn._0,
                                "eq?",
                                fn._1
                              ]);
                          return ;
                      case "Neq" :
                          console.log([
                                name,
                                fn._0,
                                "neq?",
                                fn._1
                              ]);
                          return ;
                      case "StrictEq" :
                          console.log([
                                name,
                                fn._0,
                                "strict_eq?",
                                fn._1
                              ]);
                          return ;
                      case "StrictNeq" :
                          console.log([
                                name,
                                fn._0,
                                "strict_neq?",
                                fn._1
                              ]);
                          return ;
                      case "Ok" :
                          console.log([
                                name,
                                fn._0,
                                "ok?"
                              ]);
                          return ;
                      case "Approx" :
                          console.log([
                                name,
                                fn._0,
                                "~",
                                fn._1
                              ]);
                          return ;
                      case "ApproxThreshold" :
                          console.log([
                                name,
                                fn._1,
                                "~",
                                fn._2,
                                " (",
                                fn._0,
                                ")"
                              ]);
                          return ;
                      case "ThrowAny" :
                          return ;
                      case "Fail" :
                          console.log("failed");
                          return ;
                      case "FailWith" :
                          console.log("failed: " + fn._0);
                          return ;
                      
                    }
                  }), suites);
    }
  }
  
}
`)
let val_unit = Js.Promise.resolve()

let from_promise_suites = %raw(`

function from_promise_suites(name, suites) {
  var match = $$Array.to_list(Process.argv);
  if (match) {
    if (is_mocha(undefined)) {
      describe(name, (function () {
              return List.iter((function (param) {
                            var code = param[1];
                            it(param[0], (function () {
                                    var arg1 = function (x) {
                                      handleCode(x);
                                      return val_unit;
                                    };
                                    return code.then(arg1);
                                  }));
                          }), suites);
            }));
    } else {
      console.log("promise suites");
    }
    return ;
  }
  
}
`)

let old_from_promise_suites_donotuse = (name, suites: list<(string, Js.Promise.t<_>)>) =>
  switch Array.to_list(argv) {
  | list{cmd, ..._} =>
    if is_mocha() {
      describe(name, (. ()) =>
        suites |> List.iter(((name, code)) =>
          it_promise(
            name,
            _ =>
              code |> Js.Promise.then_(
                x => {
                  handleCode(x)
                  val_unit
                },
              ),
          )
        )
      )
    } else {
      Js.log("promise suites")
    } /* TODO */
  | _ => ()
  }

/*
Note that [require] is a file local value,
we need type [require]

let is_top : unit -> bool = [%bs.raw{|
function (_){
console.log('hi');
if (typeof require === "undefined"){
  return false
} else {
  console.log("hey",require.main.filename);
  return require.main === module;
}
}
|}]

let from_pair_suites_non_top name suites =
    if not @@ is_top () then
      from_pair_suites name suites
*/

let eq_suites = (~test_id, ~suites, loc, x, y) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Eq(x, y)), ...suites.contents}
}

let bool_suites = (~test_id, ~suites, loc, x) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => Ok(x)), ...suites.contents}
}

let throw_suites = (~test_id, ~suites, loc, x) => {
  incr(test_id)
  suites :=
    list{(loc ++ (" id " ++ string_of_int(test_id.contents)), _ => ThrowAny(x)), ...suites.contents}
}
