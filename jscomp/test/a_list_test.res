let suites = {
  open Mt
  list{
    ("drop", _ => Eq(Ext_list_test.drop(3, list{0, 1, 2}), list{})),
    ("drop1", _ => Eq(Ext_list_test.drop(2, list{0, 1, 2}), list{2})),
    ("flat_map", _ => Eq(list{0, 0, 1, 1, 0}, Ext_list_test.flat_map(x =>
          if mod(x, 2) === 0 {
            list{0}
          } else {
            list{1, 1}
          }
        , list{0, 0, 3, 0}))),
  }
}

open Mt
from_pair_suites(__MODULE__, suites)
