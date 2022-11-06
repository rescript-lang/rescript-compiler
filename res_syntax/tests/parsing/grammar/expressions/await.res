await wait(2)

let maybeSomeValue = switch await fetchData(url) {    
| data => Some(data)
| exception JsError(_) => None
}

let x = await 1 + 2

let x = await wait(1) + await wait(2)

let () = {
  let response = await fetch("/users.json"); // get users list
  let users = await response.json(); // parse JSON
  let comments = (await (await fetch("comment.json")).json())[0];
  Js.log2(users, comments)
}

let () = {
    await delay(10)
}

let () = {
    await delay(10)
    await delay(20)
}