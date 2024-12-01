/***
Bindings to async iterators, a way to do async iteration in JavaScript.

See [async iterator protocols](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols#the_async_iterator_and_async_iterable_protocols) on MDN.*/

/**
The type representing an async iterator.
*/
type t<'a>

type value<'a> = {
  /**
  Whether there are more values to iterate on before the iterator is done.
  */
  done: bool,
  /**
  The value of this iteration, if any.
  */
  value: option<'a>,
}

/**
 `make(nextFn)`

  Creates an async iterator from a function that returns the next value of the iterator.

  ## Examples
  - A simple example, creating an async iterator that returns 1, 2, 3:
  ```rescript
  let context = ref(0)

  let asyncIterator = AsyncIterator.make(async () => {
    let currentValue = context.contents
    // Increment current value
    context := currentValue + 1
    
    {
      AsyncIterator.value: Some(currentValue),
      done: currentValue >= 3
    }
  })

  // This will log 1, 2, 3
  await asyncIterator->AsyncIterator.forEach(value =>
    switch value {
    | Some(value) => Console.log(value)
    | None => ()
    }
  )
  ```
 */
let make: (unit => promise<value<'value>>) => t<'value>

/**
 `value(value)`

 Shorthand for creating a value object with the provided value, and the `done` property set to false.

  ## Examples
  ```rescript
  let context = ref(0)

  let asyncIterator = AsyncIterator.make(async () => {
    let currentValue = context.contents
    // Increment current value
    context := currentValue + 1
    
    if currentValue >= 3 {
      AsyncIterator.done()
    } else {
      AsyncIterator.value(currentValue)
    }
  })
  ```
 */
let value: 'value => value<'value>

/**
 `done(~finalValue=?)`

 Shorthand for creating a value object with the `done` property set to true, and the provided value as the final value, if any.

  ## Examples
  ```rescript
  let context = ref(0)

  let asyncIterator = AsyncIterator.make(async () => {
    let currentValue = context.contents
    // Increment current value
    context := currentValue + 1
    
    if currentValue >= 3 {
      AsyncIterator.done()
    } else {
      AsyncIterator.value(currentValue)
    }
  })
  ```
 */
let done: (~finalValue: 'value=?) => value<'value>

/**
`next(asyncIterator)`

Returns the next value of the iterator, if any.

See [async iterator protocols](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols#the_async_iterator_and_async_iterable_protocols) on MDN.

## Examples
- A simple example, getting the next value:
```rescript
@val external asyncIterator: AsyncIterator.t<int> = "someAsyncIterator"
let value = await asyncIterator->AsyncIterator.next
```

- Complete example, including looping over all values:
```rescript
// Let's pretend we get an async iterator returning ints from somewhere.
@val external asyncIterator: AsyncIterator.t<int> = "someAsyncIterator"


let processMyAsyncIterator = async () => {
  // ReScript doesn't have `for ... of` loops, but it's easy to mimic using a while loop.
  let break = ref(false)

  while !break.contents {
    // Await the next iterator value
    let {value, done} = await asyncIterator->AsyncIterator.next

    // Exit the while loop if the iterator says it's done
    break := done

    // This will log the (int) value of the current async iteration, if a value was returned.
    Console.log(value)
  }
}
```
*/
@send
external next: t<'a> => promise<value<'a>> = "next"

/**
`forEach(iterator, fn)` consumes all values in the async iterator and runs the callback `fn` for each value.

See [iterator protocols](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols) on MDN.

## Examples
```rescript
// Let's pretend we get an async iterator returning ints from somewhere.
@val external asyncIterator: AsyncIterator.t<int> = "someAsyncIterator"

await asyncIterator->AsyncIterator.forEach(value =>
  switch value {
  | Some(value) if value > 10 => Console.log("More than 10!")
  | _ => ()
  }
)
```
*/
let forEach: (t<'a>, option<'a> => unit) => promise<unit>
