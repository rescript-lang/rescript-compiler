let assertion_test = () => {
  let m = ref(Int_map.empty)
  let count = 1000000
  for i in 0 to count {
    m := Int_map.add(i, i, m.contents)
  }
  for i in 0 to count {
    ignore(Int_map.find(i, m.contents))
  }
}
