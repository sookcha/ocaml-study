open Base

type bst = Empty
type node = { left : node; right : node; value : string }

let empty = Empty
let value _ = failwith "'value' is missing"
let left _ = failwith "'left' is missing"
let right _ = failwith "'right' is missing"
let insert _ _ = failwith "'insert' is missing"
let to_list _ = failwith "'to_list' is missing"
