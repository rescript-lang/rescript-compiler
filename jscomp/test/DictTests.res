let someString = "hello"

let createdDict = dict{
  "name": "hello",
  "age": "what",
  "more": "stuff",
  "otherStr": someString,
}

let three = 3

let intDict = dict{
  "one": 1,
  "two": 2,
  "three": three,
}

module PatternMatching = {
  let inferDictByPattern = dict =>
    switch dict {
    | @res.dictPattern {one: 1} =>
      let _d: dict<int> = dict
      Js.log("one")
    | _ => Js.log("not one")
    }

  let constrainedAsDict = (dict: dict<int>) =>
    switch dict {
    | @res.dictPattern {one: 1} =>
      let _d: dict<int> = dict
      Js.log("one")
    | _ => Js.log("not one")
    }
}
