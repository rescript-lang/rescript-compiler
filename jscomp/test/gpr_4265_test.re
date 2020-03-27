open Belt;
let mockMap = MutableMap.Int.make();
let add = id => {
  mockMap->MutableMap.Int.set(id, id);
  id;
};
let remove = id => {
  mockMap->MutableMap.Int.remove(id);
};

add(1726);
let n = add(6667);
add(486);
remove(1726);

let n1 = mockMap->MutableMap.Int.getExn(6667);

Js.log2("should be identical", n1 === n); //false
Js.log(n); // 6667
Js.log(n1); // 1726