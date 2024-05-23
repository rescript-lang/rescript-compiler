type t<'a>

type value<'a> = {
  done: bool,
  value: option<'a>,
}

@send external next: t<'a> => promise<value<'a>> = "next"

let forEach = async (iterator, f) => {
  let iteratorDone = ref(false)

  while !iteratorDone.contents {
    let {done, value} = await iterator->next
    f(value)
    iteratorDone := done
  }
}
