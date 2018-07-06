open Belt;;

let mySet = MutableSet.Int.make()

;; mySet
|. (
  MutableSet.Int.add(1),
  MutableSet.Int.add(2),
  MutableSet.Int.remove(1)
)
|. ignore

;; let a = 3