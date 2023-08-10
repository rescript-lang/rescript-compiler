module Test = (Queue: module type of Queue) => {
  let to_array = q => {
    let v = Array.make(Queue.length(q), 0)
    \"@@"(ignore, Queue.fold((i, e) => {
        v[i] = e
        i + 1
      }, 0, q))
    v
  }
  @val("console.log") @variadic external dump: array<'a> => unit = ""

  let queue_1 = x => {
    let q = Queue.create()
    /* dump [|q|]; */
    Array.iter(x => Queue.add(x, q), x)
    /* dump [|q|]; */
    to_array(q)
  }

  /*
    TODO: Note it needs need recursive values support */
    /* let _ = queue_1 [|38|] */
  
}

module T1 = Test(Queue)
module T2 = Test(Queue_402)
open Mt

let suites = {
  open Mt
  list{
    (
      __LOC__,
      _ => {
        let x = [3, 4, 5, 2]
        Eq(x, T1.queue_1(x))
      },
    ),
    (
      __LOC__,
      _ => {
        let x = [3, 4, 5, 2]
        Eq(x, T2.queue_1(x))
      },
    ),
  }
}

from_pair_suites(__MODULE__, suites)
