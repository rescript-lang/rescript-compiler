let for_3 = x => {
  let v = ref(0)
  let arr = Array.map((_, _) => (), x)
  for i in 0 to Array.length(x) - 1 {
    let j = i * 2
    arr[i] = _ => v := v.contents + j
  }
  Array.iter(x => x(), arr)
  v.contents
}

let for_4 = x => {
  let v = ref(0)
  let arr = Array.map((_, _) => (), x)
  for i in 0 to Array.length(x) - 1 {
    let j = i * 2
    let k = 2 * j
    arr[i] = _ => v := v.contents + k
  }
  Array.iter(x => x(), arr)
  v.contents
}

let for_5 = (x, u) => {
  let v = ref(0)
  let arr = Array.map((_, _) => (), x)
  for i in 0 to Array.length(x) - 1 {
    let _j = i * 2
    let k = 2 * u * u
    arr[i] = _ => v := v.contents + k
  }
  Array.iter(x => x(), arr)
  v.contents
}

let for_6 = (x, u) => {
  let v = ref(0)
  let arr = Array.map((_, _) => (), x)
  let v4 = ref(0)
  let v5 = ref(0)
  let inspect_3 = ref(-1)
  incr(v4)
  for j in 0 to 1 {
    incr(v5)
    let v2 = ref(0)
    let v3 = u
    for i in 0 to Array.length(x) - 1 {
      let _j = i * 2
      let k = 2 * u * u
      let h = 2 * v5.contents
      incr(v2)
      arr[i] = _ => v := v.contents + k + v2.contents + v4.contents + v5.contents + h + v3
      /* v2 should not be captured */
    }
    inspect_3 := v2.contents
  }
  Array.iter(x => x(), arr)
  [v.contents, v4.contents, v5.contents, inspect_3.contents]
}

let for_7 = () => {
  let i_len = 7
  let j_len = 3
  let v = ref(0)
  let arr = Array.make(i_len * j_len, _ => ())
  for i in 0 to i_len - 1 {
    for j in 0 to j_len - 1 {
      arr[i * j_len + j] = _ => v := v.contents + i + j
    }
  }
  Array.iter(f => f(), arr)
  v.contents
}

let for_8 = () => {
  let i_len = 7
  let j_len = 3
  let v = ref(0)
  let arr = Array.make(i_len * j_len, _ => ())
  for i in 0 to i_len - 1 {
    let k = 2 * i
    for j in 0 to j_len - 1 {
      let h = i + j
      arr[i * j_len + j] = _ => v := v.contents + i + j + h + k
    }
  }
  Array.iter(f => f(), arr)
  v.contents
}

let for_9 = () => {
  let (collect, get) = {
    let v: ref<list<int>> = ref(list{})
    (x => v := list{x, ...v.contents}, () => \"@@"(Array.of_list, List.rev(v.contents)))
  }

  let i_len = 2
  let j_len = 2
  let vv = ref(0)
  let vv2 = ref(0)
  let arr = Array.make(i_len * j_len, _ => ())
  let arr2 = Array.make(i_len, _ => ())
  for i in 0 to i_len - 1 {
    let v = ref(0)
    /* incr v ; */
    v := v.contents + i
    for j in 0 to j_len - 1 {
      incr(v)
      collect(v.contents)
      arr[i * j_len + j] = _ => vv := vv.contents + v.contents
      /* v should not be captured inside , 
           since for next iteration, 
           we are bound the same v

           there are four iterations of this function
           
           the first two bound one v 

           the second two bound the other one

           -- sometimes it's hard to tell the difference,  
           when v is not relevant to the outer [index]
           actually we have to lexical scope the whole for statement
 */
    }
    arr2[i] = _ => vv2 := vv2.contents + v.contents
    /* v should be captured, since next iteration 
        v is changed
 */
  }
  Array.iter(f => f(), arr)
  Array.iter(f => f(), arr2)
  [(vv.contents, get(), vv2.contents)]
}

/*

See how google closure works, in both simple model and advanced model

{[
var x = []

var u = []
var result = 0
for(let i = 0; i < 2; ++i){
  let counter = 0;
  counter += i;
  for(let j = 0; j < 2 ; ++j){
     x[i * 2 +j ] = ()=>{ result += counter}
   }
  u.push(counter)
}
x.forEach(x=>x())
console.log(result,u)
]}


*/

let suites = list{
  ("for_loop_test_3", _ => Mt.Eq(90, \"@@"(for_3, Array.make(10, 2)))),
  ("for_loop_test_4", _ => Mt.Eq(180, \"@@"(for_4, Array.make(10, 2)))),
  ("for_loop_test_5", _ => Mt.Eq(2420, for_5(Array.make(10, 2), 11))),
  ("for_loop_test_6", _ => Mt.Eq([30, 1, 2, 3], for_6(Array.make(3, 0), 0))),
  ("for_loop_test_7", _ => Mt.Eq(84, for_7())),
  ("for_loop_test_8", _ => Mt.Eq(294, for_8())),
  ("for_loop_test_9", _ => Mt.Eq([(10, [1, 2, 2, 3], 5)], for_9())),
}
