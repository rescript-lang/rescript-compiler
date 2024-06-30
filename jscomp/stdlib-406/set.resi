/***
Bindings to the mutable JavaScript `Set`.

See [`Set`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set) on MDN.
*/

/**
Type representing an instance of `Set`.
*/
type t<'a> = Js.Set.t<'a>

/**
Creates a new, mutable JavaScript `Set`. A `Set` is a collection of unique values.

See [`Set`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set) on MDN.



## Examples
```rescript
// You can annotate the type of your set if you want to
let mySet: Set.t<string> = Set.make()

// Or you can let ReScript infer what's in your Set
let set = Set.make()
set->Set.add("Fine name") // Inferred as Set.t<string>
```

## Alternatives
A JavaScript `Set` is mutable. If you're looking for an immutable alternative, check out `Belt.Set`.
*/
@new
external make: unit => t<'a> = "Set"

/**
Turns an array of values into a Set. Meaning only unique values are preserved.

## Examples
```rescript
type languages = ReScript | JavaScript | TypeScript
let languageRank = [ReScript, JavaScript, TypeScript]

let set = Set.fromArray(languageRank) // Set.t<languages>

switch set->Set.has(ReScript) {
| true => Console.log("Yay, ReScript is in there!")
| false => Console.log("Uh-oh, something is _terribly_ wrong with this program... abort.")
}
```
*/
@new
external fromArray: array<'a> => t<'a> = "Set"

/**
Turns an iterator into a `Set`.

## Examples
```rescript
// Let's pretend we have an interator
@val external someIterator: Iterator.t<int> = "someIterator"

let set = Set.fromIterator(someIterator) // Set.t<int>
```
*/
@new
external fromIterator: Iterator.t<'a> => t<'a> = "Set"

/**
Returns the size, the number of unique values, of the set.

## Examples
```rescript
let set = Set.make()

set->Set.add("someValue")
set->Set.add("someValue")
set->Set.add("someValue2")

let size = set->Set.size // 2
```
*/
@get
external size: t<'a> => int = "size"

/**
Clears all entries in the set.

## Examples
```rescript
let set = Set.make()

set->Set.add("someKey")
set->Set.size // 1

set->Set.clear
set->Set.size // 0
```
*/
@send
external clear: t<'a> => unit = "clear"

/**
Adds a new value to the set.

## Examples
```rescript
let set = Set.make()
set->Set.add("someValue")
```
*/
@send
external add: (t<'a>, 'a) => unit = "add"

/**
Deletes the provided `value` from the set. Returns a `bool` for whether the value existed, and was deleted.

## Examples
```rescript
let set = Set.make()
set->Set.add("someValue")
let didDeleteValue = set->Set.delete("someValue")
Console.log(didDeleteValue) // Logs `true` to the console, becuase the set had the value, so it was successfully deleted

let didDeleteValue = set->Set.delete("someNonExistantKey")
Console.log(didDeleteValue) // Logs `false` to the console, becuase the value did not exist in the set
```
*/
@send
external delete: (t<'a>, 'a) => bool = "delete"

/**
Checks whether the set has a specific value.

## Examples
```rescript
let set = Set.make()
set->Set.add("someValue")

switch set->Set.has("someValue") {
| false => Console.log("Nope, didn't have it.")
| true => Console.log("Yay, we have the value!")
}
```
*/
@send
external has: (t<'a>, 'a) => bool = "has"

/**
Iterates through all values of the set.

## Examples
```rescript
let set = Set.make()
set->Set.add("someValue")
set->Set.add("someValue2")

set->Set.forEach(value => {
  Console.log(value)
})
```
*/
@send
external forEach: (t<'a>, 'a => unit) => unit = "forEach"

/**
Returns an iterator that holds all values of the set.

## Examples
```rescript
let set = Set.make()
set->Set.add("someValue")
set->Set.add("anotherValue")

let values = set->Set.values

// Logs the first value
Console.log(Iterator.next(values).value)

// You can also turn the iterator into an array.
// Remember that an iterator consumes values. We'll need a fresh values iterator to get an array of all values, since we consumed a value via `next` above already.
Console.log(set->Set.values->Iterator.toArray)
```
*/
@send
external values: t<'a> => Iterator.t<'a> = "values"
